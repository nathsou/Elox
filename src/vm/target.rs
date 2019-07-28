use super::wasm_module::{
    encode_vec, DataInitializer, Encoder, FuncBody, FuncIdx, FuncSignature, Global, Initializer,
    Locals, MemoryIdx, ValueType, WasmFunc, WasmInst, WasmModule,
};

extern crate byteorder;
use byteorder::{ByteOrder, LittleEndian};

use super::{Chunk, EloxVM, Inst, Value};
use crate::parser::Identifier;
use crate::runner::{EloxResult, EloxRunner};
use crate::scanner::scanner_result::ErrorPosition;
use fnv::FnvHashMap;

pub trait EloxTranslator {
    fn translate(&mut self, chunk: &Chunk) -> Vec<u8>;
}

pub struct WasmTarget {
    module: WasmModule,
    code: Vec<WasmInst>,
    used_funcs: FnvHashMap<FuncUtil, FuncIdx>,
    imports_count: usize,
}

impl WasmTarget {
    pub fn new() -> WasmTarget {
        WasmTarget {
            module: WasmModule::new(),
            code: vec![],
            used_funcs: FnvHashMap::default(),
            imports_count: 1, // print
        }
    }

    fn emit(&mut self, inst: WasmInst) {
        self.code.push(inst);
    }

    fn emit_multiple(&mut self, insts: &[WasmInst]) {
        for inst in insts {
            self.emit(inst.clone());
        }
    }

    fn use_(&mut self, func: FuncUtil) -> FuncIdx {
        if let Some(idx) = self.used_funcs.get(&func) {
            return *idx + self.imports_count + 1;
        }

        let idx = self.used_funcs.len();
        self.used_funcs.insert(func, idx);

        idx + self.imports_count + 1 // + main
    }

    fn call(&mut self, func: FuncUtil) -> WasmInst {
        WasmInst::Call(self.use_(func))
    }

    fn translate_inst(&mut self, inst: &Inst, chunk: &Chunk, func_args: usize) {
        let local_idx_offset = -(Identifier::reserved_count() as isize) + func_args as isize;
        let wasm_inst = match inst {
            Inst::Const(idx) => {
                let val = chunk.const_(*idx).unwrap();
                self.emit_multiple(&[
                    //WasmInst::Consti32(val.type_nb() as i32),
                    match val {
                        Value::Number(n) => WasmInst::Constf64(*n),
                        Value::Boolean(b) => WasmInst::Consti32(if *b { 1 } else { 0 }),
                        _ => panic!("const not implemented: {:?}", val),
                    },
                ]);
                return;
            }
            // Inst::Const(idx) => WasmInst::Consti32(*idx as i32),
            Inst::Print => WasmInst::Call(0),
            Inst::Add => WasmInst::Addf64,
            Inst::Sub => WasmInst::Subf64,
            Inst::Mult => WasmInst::Mulf64,
            Inst::Div => WasmInst::Divf64,
            Inst::Neg => WasmInst::Negf64,
            Inst::Mod => self.call(FuncUtil::ModF64),
            Inst::Pop => WasmInst::Drop_,
            Inst::True => WasmInst::Consti32(1),
            Inst::False => WasmInst::Consti32(0),
            Inst::GetLocal(idx) => WasmInst::GetLocal((*idx as isize + local_idx_offset) as u32),
            Inst::SetLocal(idx) => WasmInst::SetLocal((*idx as isize + local_idx_offset) as u32),
            Inst::GetGlobal(idx) => WasmInst::GetLocal((*idx as isize + local_idx_offset) as u32),
            Inst::SetGlobal(idx) | Inst::DefGlobal(idx) => {
                WasmInst::SetLocal((*idx as isize + local_idx_offset) as u32)
            }
            Inst::Ret => return,
            _ => panic!(format!("wasm translation not implemented for {:?}", inst)),
        };

        self.emit(wasm_inst);
    }
}

#[derive(Hash, PartialEq, Eq)]
enum FuncUtil {
    ModF64,
    Add,
}

struct WasmUtils;

impl WasmUtils {
    pub fn get(func: &FuncUtil) -> WasmFunc {
        match func {
            FuncUtil::ModF64 => WasmUtils::f64mod(),
            FuncUtil::Add => WasmUtils::add(),
        }
    }

    fn f64mod() -> WasmFunc {
        use ValueType::*;
        use WasmInst::*;
        WasmFunc::new(
            FuncSignature::new(&[F64, F64], Some(F64)),
            FuncBody::new(
                Locals::from_types(&[]),
                &[
                    GetLocal(0),
                    GetLocal(1),
                    GetLocal(0),
                    GetLocal(1),
                    Divf64,
                    Floorf64,
                    Mulf64,
                    Subf64,
                ],
            ),
        )
    }

    fn add() -> WasmFunc {
        use ValueType::*;
        use WasmInst::*;
        WasmFunc::new(
            FuncSignature::new(&[I32, F64, I32, F64], Some(F64)),
            FuncBody::new(Locals::from_types(&[]), &[GetLocal(1), GetLocal(3), Addf64]),
        )
    }
}

impl Encoder for Value {
    fn encode(&self) -> Vec<u8> {
        match self {
            Value::Number(n) => {
                let mut encoded = vec![1u8]; // number type
                let mut buf = [0; 8];
                LittleEndian::write_f64(&mut buf, *n);
                encoded.extend(&buf);

                encoded
            }
            _ => panic!(format!("type not encodable yet: {:?}", self)),
        }
    }
}

impl Value {
    pub fn to_data_initializer(&self, mem_idx: MemoryIdx, offset: u32) -> (DataInitializer, u32) {
        let mut encoded = self.encode();
        encoded = encode_vec(encoded);
        let len = encoded.len() as u32;
        (
            DataInitializer::new(mem_idx, Initializer::Consti32(offset as i32), &encoded),
            len,
        )
    }

    pub fn type_nb(&self) -> u8 {
        match self {
            Value::Number(_) => 0x01,
            Value::Boolean(_) => 0x02,
            Value::Nil => 0x03,
            Value::Object(_) => 0x04,
        }
    }
}

impl EloxTranslator for WasmTarget {
    fn translate(&mut self, chunk: &Chunk) -> Vec<u8> {
        use ValueType::*;

        self.module
            .import_func("host", "print", FuncSignature::new(&[F64], None));

        // minimum if all constants are numbers (1 type byte + 8 data bytes)
        let mut constants_data: Vec<u8> = Vec::with_capacity(9 * chunk.constants().len());

        for constant in chunk.constants() {
            constants_data.extend(&constant.encode());
        }

        constants_data = encode_vec(encode_vec(constants_data));

        self.module.add_data_init(DataInitializer::new(
            0,
            Initializer::Consti32(0i32),
            &constants_data,
        ));

        for inst in chunk.instructions() {
            self.translate_inst(inst, chunk, 0);
        }

        let start_func = self.module.add_func(WasmFunc::new(
            FuncSignature::void(),
            FuncBody::new(Locals::from_types(&[I32]), &self.code),
        ));

        self.module.set_start_func(start_func);

        for used_func in self.used_funcs.keys() {
            self.module.add_func(WasmUtils::get(used_func));
        }

        // set the first global to the memory offset so far
        self.module.add_global(Global::new(
            ValueType::I32,
            Initializer::Consti32(constants_data.len() as i32),
        ));

        self.module.encode()
    }
}

impl EloxRunner for WasmTarget {
    fn run(&mut self, source: &str) -> EloxResult {
        let mut vm = EloxVM::new();
        vm.compile(source)?;
        let bytes = self.translate(vm.chunk());

        use std::fs::File;
        use std::io::prelude::*;

        let mut out = File::create("out.wasm").expect("could not create file");
        out.write_all(&bytes).expect("could not write to file");

        Ok(())
    }

    fn throw_error(&mut self, err: impl ErrorPosition) -> EloxResult {
        let pos = err.position();
        println!("Error [line {}:{}]: {}", pos.line, pos.col, err);
        Ok(())
    }
}
