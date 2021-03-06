extern crate byteorder;
use byteorder::{ByteOrder, LittleEndian};

// Encoding:
// https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#type-section

// https://webassembly.github.io/spec/core/binary/instructions.html
#[derive(Copy, Clone)]
enum WasmSection {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
}

// https://webassembly.github.io/spec/core/binary/types.html
#[derive(Copy, Clone, PartialEq)]
pub enum ValueType {
    I32 = 0x7f,
    I64 = 0x7e,
    F64 = 0x7c,
}

// unsigned LEB128
// https://www.wikiwand.com/en/LEB128
fn uleb128(n: usize) -> Vec<u8> {
    let mut bytes = vec![];
    let mut val = n;

    loop {
        let mut byte = (val & 0x7F) as u8;
        val >>= 7;

        if val != 0 {
            byte |= 0x80;
            bytes.push(byte);
        } else {
            bytes.push(byte);
            break;
        }
    }

    bytes
}

// signed LEB128
// https://doc.rust-lang.org/nightly/nightly-rustc/src/serialize/leb128.rs.html#112-114
fn sleb128(n: isize) -> Vec<u8> {
    let mut bytes = vec![];
    let mut value = n;

    loop {
        let mut byte = (value as u8) & 0x7f;
        value >>= 7;
        let more =
            !(((value == 0) && ((byte & 0x40) == 0)) || ((value == -1) && ((byte & 0x40) != 0)));

        if more {
            byte |= 0x80; // Mark this byte to show that more bytes will follow.
        }

        bytes.push(byte);

        if !more {
            break;
        }
    }

    bytes
}

pub trait Encoder {
    fn encode(&self) -> Vec<u8>;
}

impl Encoder for Vec<u8> {
    fn encode(&self) -> Vec<u8> {
        let mut encoded = Vec::with_capacity(self.len() + 1);
        encoded.extend(&uleb128(self.len()));
        encoded.extend(self);

        encoded
    }
}

macro_rules! flat_vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            flatten(temp_vec)
        }
    };
}

#[derive(PartialEq)]
pub struct FuncSignature {
    params: Vec<ValueType>,
    ret: Vec<ValueType>,
}

impl FuncSignature {
    pub fn new(params: &[ValueType], ret: Option<ValueType>) -> FuncSignature {
        FuncSignature {
            params: params.to_vec(),
            ret: if let Some(ret) = ret {
                vec![ret]
            } else {
                vec![]
            },
        }
    }

    /// a function with no parameters and which returns nothing
    pub fn void() -> FuncSignature {
        FuncSignature::new(&[], None)
    }
}

// http://webassembly.github.io/spec/core/binary/types.html#function-types
const FUNC_TYPE: u8 = 0x60;

impl Encoder for FuncSignature {
    fn encode(&self) -> Vec<u8> {
        let params = &self.params.iter().map(|&p| p as u8).collect::<Vec<_>>();
        let ret = &self.ret.iter().map(|&r| r as u8).collect::<Vec<_>>();

        flat_vec![vec![FUNC_TYPE], params.encode(), ret.encode()]
    }
}

fn flatten<T>(vecs: Vec<Vec<T>>) -> Vec<T> {
    vecs.into_iter().flatten().collect()
}

pub fn encode_vec(v: Vec<u8>) -> Vec<u8> {
    let mut encoded = Vec::with_capacity(v.len() + 1);
    encoded.extend(&uleb128(v.len()));
    encoded.extend(v);

    encoded
}

fn encode_vecs(vecs: Vec<Vec<u8>>) -> Vec<u8> {
    let mut encoded = Vec::with_capacity(vecs.len() + 1);
    encoded.extend(&uleb128(vecs.len()));
    encoded.extend(flatten(vecs));

    encoded
}

impl Encoder for u32 {
    fn encode(&self) -> Vec<u8> {
        let mut buf = [0; 4];
        LittleEndian::write_u32(&mut buf, *self);
        buf.to_vec()
    }
}

fn encode_section(idx: WasmSection, content: Vec<u8>) -> Vec<u8> {
    flat_vec![vec![idx as u8], encode_vec(content)]
}

pub type FuncSignatureIdx = usize;

// The Type Section consists of an array of function signatures.
struct TypeSection {
    pub signatures: Vec<FuncSignature>,
}

impl TypeSection {
    pub fn new() -> TypeSection {
        TypeSection { signatures: vec![] }
    }

    // returns the idx of the signature
    pub fn add(&mut self, signature: FuncSignature) -> FuncSignatureIdx {
        for (idx, sig) in self.signatures.iter().enumerate() {
            if sig == &signature {
                return idx;
            }
        }

        self.signatures.push(signature);
        self.signatures.len() - 1
    }
}

impl Encoder for TypeSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Type,
            encode_vecs(self.signatures.iter().map(|sig| sig.encode()).collect()),
        )
    }
}

// The Function Section consists of an array of function declarations.
// Its elements directly correspond to elements in the Code Section array.

// A function declaration consists of:
// an index in the Type Section of the signature of the function.
struct FuncSection {
    pub indices: Vec<FuncSignatureIdx>,
}

impl FuncSection {
    pub fn new() -> FuncSection {
        FuncSection { indices: vec![] }
    }

    // indexes a func signature from the type section
    pub fn add_signature(&mut self, signature_idx: FuncSignatureIdx) -> FuncIdx {
        self.indices.push(signature_idx);
        self.indices.len() - 1
    }
}

impl Encoder for FuncSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Function,
            encode_vecs(
                self.indices
                    .iter()
                    .map(|&idx| uleb128(idx))
                    .collect::<Vec<_>>(),
            ),
        )
    }
}

pub struct Locals {
    count: usize,     // number of local variables of the following type
    type_: ValueType, // type of the variables
}

impl Locals {
    // run-length sequence of types
    pub fn from_types(types: &[ValueType]) -> Vec<Locals> {
        let mut locals = vec![];

        match types.len() {
            0 => {}
            1 => {
                locals.push(Locals {
                    count: 1,
                    type_: types[0],
                });
            }
            _ => {
                let mut prev_type = types[0];
                let mut count = 1;

                for (idx, &type_) in types.iter().skip(1).enumerate() {
                    if prev_type == type_ {
                        count += 1;
                    } else {
                        locals.push(Locals {
                            count,
                            type_: prev_type,
                        });
                        prev_type = type_;
                        count = 1;
                    }
                    // we skipped the first one
                    if idx == types.len() - 2 {
                        locals.push(Locals { count, type_ });
                    }
                }
            }
        }

        locals
    }
}

impl Encoder for Locals {
    fn encode(&self) -> Vec<u8> {
        let mut encoded = vec![];
        encoded.extend(&uleb128(self.count));
        encoded.push(self.type_ as u8);

        encoded
    }
}

impl Encoder for Vec<Locals> {
    fn encode(&self) -> Vec<u8> {
        encode_vecs(
            self.iter()
                .map(|locals| locals.encode())
                .collect::<Vec<_>>(),
        )
    }
}

// http://webassembly.github.io/spec/core/appendix/index-instructions.html
#[derive(Clone)]
pub enum WasmInst {
    Consti32(i32),
    Constf64(f64),
    GetLocal(u32),
    SetLocal(u32),
    GetGlobal(u32),
    SetGlobal(u32),
    End,
    Addf64,
    Subf64,
    Mulf64,
    Divf64,
    Floorf64,
    Negf64,
    RemUi64,
    TruncF64ToI64,
    ConvertI64ToF64,
    Drop_,
    Call(FuncIdx),
    Loadf64(u32),
    Loadi32(u32),
    Storef64(u32),
    Storei32(u32),
}

impl WasmInst {
    pub fn opcode(&self) -> u8 {
        use WasmInst::*;
        match self {
            Consti32(_) => 0x41,
            Constf64(_) => 0x44,
            GetLocal(_) => 0x20,
            SetLocal(_) => 0x21,
            GetGlobal(_) => 0x23,
            SetGlobal(_) => 0x24,
            End => 0x0b,
            Addf64 => 0xa0,
            Subf64 => 0xa1,
            Mulf64 => 0xa2,
            Divf64 => 0xa3,
            Floorf64 => 0x9c,
            Negf64 => 0x9a,
            RemUi64 => 0x82,
            TruncF64ToI64 => 0xb0,
            ConvertI64ToF64 => 0xb9,
            Drop_ => 0x1a,
            Call(_) => 0x10,
            Loadf64(_) => 0x2b,
            Loadi32(_) => 0x28,
            Storef64(_) => 0x39,
            Storei32(_) => 0x36,
        }
    }
}

pub struct WasmFunc {
    signature: FuncSignature,
    body: FuncBody,
}

impl WasmFunc {
    pub fn new(signature: FuncSignature, body: FuncBody) -> WasmFunc {
        WasmFunc { signature, body }
    }
}

impl Encoder for WasmInst {
    fn encode(&self) -> Vec<u8> {
        use WasmInst::*;
        match self {
            Consti32(val) => flat_vec![vec![self.opcode()], sleb128(*val as isize)],
            Constf64(val) => {
                let mut buf = [0; 8];
                LittleEndian::write_f64(&mut buf, *val);
                flat_vec![vec![self.opcode()], buf.to_vec()]
            }
            GetLocal(idx) | SetLocal(idx) => flat_vec![vec![self.opcode()], uleb128(*idx as usize)],
            Call(func_idx) => flat_vec![vec![self.opcode()], uleb128(*func_idx)],
            Loadi32(idx) | Loadf64(idx) | Storef64(idx) | Storei32(idx) => {
                flat_vec![vec![self.opcode()], uleb128(*idx as usize)]
            }
            _ => vec![self.opcode()],
        }
    }
}

impl Encoder for Vec<WasmInst> {
    fn encode(&self) -> Vec<u8> {
        flatten(self.iter().map(|inst| inst.encode()).collect())
    }
}

// A function body consists of:

// Field Name	Type	Description
// body_size	varuint32	the size of locals and instructions, in bytes
// locals	array of local entry	local variable declarations
// instructions	sequence of instructions	the instructions
pub struct FuncBody {
    locals: Vec<Locals>, // local variable declarations
    instructions: Vec<WasmInst>,
}

impl FuncBody {
    pub fn new(locals: Vec<Locals>, instructions: &[WasmInst]) -> FuncBody {
        FuncBody {
            locals,
            instructions: instructions.to_vec(),
        }
    }
}

impl Encoder for FuncBody {
    fn encode(&self) -> Vec<u8> {
        let locals = self.locals.encode();
        let mut instructions = self.instructions.encode();
        instructions.push(WasmInst::End.opcode());

        let mut encoded = uleb128(locals.len() + instructions.len());
        encoded.extend(locals);
        encoded.extend(instructions);

        encoded
    }
}

pub type FuncIdx = usize;

// The Code Section consists of an array of function bodies.
struct CodeSection {
    func_bodies: Vec<FuncBody>,
}

impl CodeSection {
    pub fn new() -> CodeSection {
        CodeSection {
            func_bodies: vec![],
        }
    }

    pub fn add_func(&mut self, body: FuncBody) -> FuncIdx {
        self.func_bodies.push(body);
        self.func_bodies.len() - 1
    }
}

impl Encoder for CodeSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Code,
            encode_vecs(
                self.func_bodies
                    .iter()
                    .map(|func_body| func_body.encode())
                    .collect::<Vec<_>>(),
            ),
        )
    }
}

pub struct WasmModule {
    type_section: TypeSection,
    import_section: Option<ImportSection>,
    func_section: FuncSection,
    memory_section: MemorySection,
    globals: GlobalSection,
    start_section: Option<StartSection>,
    code_section: CodeSection,
    data_section: DataSection,
    imports_count: usize,
}

impl WasmModule {
    pub fn new() -> WasmModule {
        WasmModule {
            type_section: TypeSection::new(),
            import_section: None,
            func_section: FuncSection::new(),
            memory_section: MemorySection::default(),
            globals: GlobalSection::new(),
            start_section: None,
            code_section: CodeSection::new(),
            data_section: DataSection::new(),
            imports_count: 0,
        }
    }

    // returns the idx of the function
    pub fn add_func(&mut self, func: WasmFunc) -> FuncIdx {
        let sig_idx = self.type_section.add(func.signature);
        self.func_section.add_signature(sig_idx);
        self.code_section.add_func(func.body) + self.imports_count
    }

    pub fn set_start_func(&mut self, start_func_idx: FuncIdx) {
        self.start_section = Some(StartSection::new(start_func_idx))
    }

    pub fn import_func(&mut self, module: &str, field: &str, signature: FuncSignature) {
        if self.import_section.is_none() {
            self.import_section = Some(ImportSection::new());
        }

        let sig_idx = self.type_section.add(signature);

        let imports = self.import_section.as_mut().unwrap();

        let entry = FuncImportEntry::new(module, field, sig_idx);
        imports.add_func(entry);
        self.imports_count += 1;
    }

    pub fn add_data_init(&mut self, data_initializer: DataInitializer) {
        self.data_section.add(data_initializer);
    }

    pub fn add_global(&mut self, global: Global) -> GlobalIdx {
        self.globals.add(global)
    }
}

impl<E: Encoder> Encoder for Option<E> {
    fn encode(&self) -> Vec<u8> {
        if let Some(encoder) = self {
            encoder.encode()
        } else {
            vec![]
        }
    }
}

impl Encoder for WasmModule {
    fn encode(&self) -> Vec<u8> {
        let encoded = flat_vec![
            vec![
                0x00, 0x61, 0x73, 0x6d, // magic cookie "\0asm"
                0x01, 0x00, 0x00, 0x00 // wasm version
            ],
            self.type_section.encode(),
            self.import_section.encode(),
            self.func_section.encode(),
            self.memory_section.encode(),
            self.globals.encode(),
            self.start_section.encode(),
            self.code_section.encode(),
            self.data_section.encode()
        ];

        encoded
    }
}

// https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#start-section
struct StartSection {
    start_func_idx: usize,
}

impl StartSection {
    pub fn new(start_func_idx: usize) -> StartSection {
        StartSection { start_func_idx }
    }
}

impl Encoder for StartSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(WasmSection::Start, uleb128(self.start_func_idx))
    }
}

// https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#import-section
struct ImportSection {
    func_entries: Vec<FuncImportEntry>,
}

impl ImportSection {
    pub fn new() -> ImportSection {
        ImportSection {
            func_entries: vec![],
        }
    }

    pub fn add_func(&mut self, f: FuncImportEntry) {
        self.func_entries.push(f);
    }
}

impl Encoder for ImportSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Import,
            encode_vecs(
                self.func_entries
                    .iter()
                    .map(|entry| entry.encode())
                    .collect(),
            ),
        )
    }
}

#[derive(Clone, Copy)]
enum ExternalKind {
    Func = 0,
    // Table = 1,
    // Memory = 2,
    // Global = 3,
}

struct ImportEntry {
    module_name: String,
    field_name: String,
    kind: ExternalKind,
}

impl Encoder for String {
    fn encode(&self) -> Vec<u8> {
        self.clone().into_bytes()
    }
}

impl Encoder for ImportEntry {
    fn encode(&self) -> Vec<u8> {
        let module_name = self.module_name.encode();
        let field_name = self.field_name.encode();

        flat_vec![
            uleb128(module_name.len()), // module_len
            module_name,
            uleb128(field_name.len()), // module_len
            field_name,
            vec![self.kind as u8]
        ]
    }
}

struct FuncImportEntry {
    import_entry: ImportEntry,
    func_type: FuncSignatureIdx,
}

impl FuncImportEntry {
    pub fn new(module_name: &str, field: &str, signature: FuncSignatureIdx) -> FuncImportEntry {
        FuncImportEntry {
            import_entry: ImportEntry {
                module_name: module_name.into(),
                field_name: field.into(),
                kind: ExternalKind::Func,
            },
            func_type: signature,
        }
    }
}

impl Encoder for FuncImportEntry {
    fn encode(&self) -> Vec<u8> {
        flat_vec![self.import_entry.encode(), uleb128(self.func_type)]
    }
}

pub enum Initializer {
    Constf64(f64),
    Consti32(i32),
    GetGlobal(u32),
}

impl Initializer {
    pub fn to_wasm_inst(&self) -> WasmInst {
        match self {
            Initializer::Constf64(val) => WasmInst::Constf64(*val),
            Initializer::GetGlobal(idx) => WasmInst::GetGlobal(*idx),
            Initializer::Consti32(idx) => WasmInst::Consti32(*idx),
        }
    }
}

impl Encoder for Initializer {
    fn encode(&self) -> Vec<u8> {
        let mut encoded = self.to_wasm_inst().encode();
        encoded.push(WasmInst::End.opcode());
        encoded
    }
}

// https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md#global-description
pub struct Global {
    type_: ValueType,
    mutable: bool,
    initializer: Initializer,
}

impl Global {
    fn new_(type_: ValueType, init: Initializer, mutable: bool) -> Global {
        Global {
            type_,
            mutable,
            initializer: init,
        }
    }

    pub fn new_immutable(type_: ValueType, init: Initializer) -> Global {
        Global::new_(type_, init, false)
    }

    pub fn new(type_: ValueType, init: Initializer) -> Global {
        Global::new_(type_, init, true)
    }
}

impl Encoder for Global {
    fn encode(&self) -> Vec<u8> {
        flat_vec![
            vec![self.type_ as u8, if self.mutable { 1 } else { 0 }],
            self.initializer.encode()
        ]
    }
}

struct GlobalSection {
    globals: Vec<Global>,
}

pub type GlobalIdx = usize;

impl GlobalSection {
    pub fn new() -> GlobalSection {
        GlobalSection { globals: vec![] }
    }

    pub fn add(&mut self, global: Global) -> GlobalIdx {
        self.globals.push(global);
        self.globals.len() - 1
    }
}

impl Encoder for GlobalSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Global,
            encode_vecs(self.globals.iter().map(|g| g.encode()).collect()),
        )
    }
}

struct ResizableLimits {
    flags: u32,
    min: u32,
}

impl Default for ResizableLimits {
    fn default() -> ResizableLimits {
        ResizableLimits { flags: 0, min: 1 }
    }
}

impl Encoder for ResizableLimits {
    fn encode(&self) -> Vec<u8> {
        flat_vec![uleb128(self.flags as usize), uleb128(self.min as usize)]
    }
}

struct LinearMemoryDescription {
    limits: ResizableLimits,
}

impl Default for LinearMemoryDescription {
    fn default() -> LinearMemoryDescription {
        LinearMemoryDescription {
            limits: ResizableLimits::default(),
        }
    }
}

impl Encoder for LinearMemoryDescription {
    fn encode(&self) -> Vec<u8> {
        self.limits.encode()
    }
}

struct MemorySection {
    descriptions: Vec<LinearMemoryDescription>,
}

impl MemorySection {
    pub fn new(description: LinearMemoryDescription) -> MemorySection {
        MemorySection {
            descriptions: vec![description],
        }
    }
}

impl Default for MemorySection {
    fn default() -> MemorySection {
        MemorySection::new(LinearMemoryDescription::default())
    }
}

impl Encoder for MemorySection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Memory,
            encode_vecs(self.descriptions.iter().map(|m| m.encode()).collect()),
        )
    }
}

pub type MemoryIdx = usize;

pub struct DataInitializer {
    mem_idx: MemoryIdx,
    offset: Initializer,
    data: Vec<u8>,
}

impl DataInitializer {
    pub fn new(mem_idx: MemoryIdx, offset: Initializer, data: &[u8]) -> DataInitializer {
        DataInitializer {
            mem_idx,
            offset,
            data: data.to_vec(),
        }
    }
}

impl Encoder for DataInitializer {
    fn encode(&self) -> Vec<u8> {
        flat_vec![
            uleb128(self.mem_idx),
            self.offset.encode(),
            self.data.clone()
        ]
    }
}

// https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md#data-section
struct DataSection {
    data_initializers: Vec<DataInitializer>,
}

impl DataSection {
    pub fn new() -> DataSection {
        DataSection {
            data_initializers: vec![],
        }
    }

    pub fn add(&mut self, initializer: DataInitializer) {
        self.data_initializers.push(initializer);
    }
}

impl Encoder for DataSection {
    fn encode(&self) -> Vec<u8> {
        encode_section(
            WasmSection::Data,
            encode_vecs(
                self.data_initializers
                    .iter()
                    .map(|init| init.encode())
                    .collect(),
            ),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn leb128() {
        assert_eq!(uleb128(624485), vec![0xe5, 0x8e, 0x26]);
        assert_eq!(sleb128(-123456), vec![0xc0, 0xbb, 0x78]);
    }

    #[test]
    fn type_section() {
        use ValueType::F64;
        let sig = FuncSignature::new(&[F64, F64], Some(F64));
        assert_eq!(sig.encode(), vec![0x60, 0x2, 0x7c, 0x7c, 0x1, 0x7c]);
        let mut type_section = TypeSection::new();
        type_section.add(sig);

        assert_eq!(
            type_section.encode(),
            vec![0x1, 0x7, 0x1, 0x60, 0x2, 0x7c, 0x7c, 0x1, 0x7c]
        );
    }

    #[test]
    fn func_section() {
        let mut func_section = FuncSection::new();
        func_section.add_signature(0);
        assert_eq!(func_section.encode(), vec![0x3, 0x2, 0x1, 0x0]);
    }

    #[test]
    fn code_section() {
        use WasmInst::*;
        let mut code_section = CodeSection::new();
        code_section.add_func(FuncBody::new(
            Locals::from_types(&[]),
            &[GetLocal(0u32), GetLocal(1u32), Addf64],
        ));

        assert_eq!(
            code_section.encode(),
            vec![0xa, 0x9, 0x1, 0x7, 0x0, 0x20, 0x0, 0x20, 0x1, 0xa0, 0xb]
        );
    }

    #[test]
    fn module() {
        let mut module = WasmModule::new();
        use ValueType::F64;
        use WasmInst::*;
        module.add_func(WasmFunc::new(
            FuncSignature::new(&[F64, F64], Some(F64)),
            FuncBody::new(
                Locals::from_types(&[]),
                &[GetLocal(0u32), GetLocal(1u32), Addf64],
            ),
        ));

        assert_eq!(
            module.encode(),
            vec![
                0x00, 0x61, 0x73, 0x6d, // magic cookie "\0asm"
                0x01, 0x00, 0x00, 0x00, // wasm version
                0x1, 0x7, 0x1, 0x60, 0x2, 0x7c, 0x7c, 0x1, 0x7c, // type section
                0x3, 0x2, 0x1, 0x0, // func section
                0xa, 0x9, 0x1, 0x7, 0x0, 0x20, 0x0, 0x20, 0x1, 0xa0, 0xb // code section
            ]
        );
    }
}
