use super::wasm_module::{
    Encoder, FuncBody, FuncIdx, FuncSignature, Locals, ValueType, WasmFunc, WasmInst, WasmModule,
};
use super::{Chunk, EloxVM, Inst, Value};
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

    fn translate_inst(&mut self, inst: &Inst, chunk: &Chunk) {
        let wasm_inst = match inst {
            Inst::Const(idx) => {
                let val = chunk.const_(*idx).unwrap();
                match val {
                    Value::Number(n) => WasmInst::Constf64(*n),
                    Value::Boolean(b) => WasmInst::Consti32(if *b { 1 } else { 0 }),
                    _ => panic!("const not implemented: {:?}", val),
                }
            }
            Inst::Print => WasmInst::Call(0),
            Inst::Add => WasmInst::Addf64,
            Inst::Sub => WasmInst::Subf64,
            Inst::Mult => WasmInst::Mulf64,
            Inst::Div => WasmInst::Divf64,
            Inst::Neg => WasmInst::Negf64,
            Inst::Mod => {
                let mod_idx = self.use_(FuncUtil::ModF64F64);
                self.emit_multiple(&[WasmInst::Call(mod_idx)]);
                return;
            }
            Inst::Pop => WasmInst::Drop_,
            Inst::Ret => return,
            _ => panic!(format!("wasm translation not implemented for {:?}", inst)),
        };

        self.emit(wasm_inst);
    }
}

#[derive(Hash, PartialEq, Eq)]
enum FuncUtil {
    ModF64F64,
}

struct WasmUtils;

impl WasmUtils {
    pub fn get(func: &FuncUtil) -> WasmFunc {
        match func {
            FuncUtil::ModF64F64 => WasmUtils::mod_f64_f64(),
        }
    }

    fn mod_f64_f64() -> WasmFunc {
        use WasmInst::*;
        WasmFunc::new(
            FuncSignature::new(&[ValueType::F64, ValueType::F64], Some(ValueType::F64)),
            FuncBody::new(
                Locals::from_types(&[]),
                &[
                    GetLocal(0),
                    TruncF64ToI64,
                    GetLocal(1),
                    TruncF64ToI64,
                    RemUi64,
                    ConvertI64ToF64,
                ],
            ),
        )
    }
}

impl EloxTranslator for WasmTarget {
    fn translate(&mut self, chunk: &Chunk) -> Vec<u8> {
        use ValueType::*;

        self.module
            .import_func("host", "print", FuncSignature::new(&[F64], None));

        for inst in chunk.instructions() {
            self.translate_inst(inst, chunk);
        }

        let start_func = self.module.add_func(WasmFunc::new(
            FuncSignature::void(),
            FuncBody::new(Locals::from_types(&[]), &self.code),
        ));

        self.module.set_start_func(start_func);

        for used_func in self.used_funcs.keys() {
            self.module.add_func(WasmUtils::get(used_func));
        }

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

    fn throw_error(&mut self, err: impl ErrorPosition) {
        let pos = err.position();
        println!("Error [line {}:{}]: {}", pos.line, pos.col, err);
    }
}
