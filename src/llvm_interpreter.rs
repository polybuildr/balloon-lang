use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::mem;

use ast::*;
use environment::Environment;
use runtime::*;

// use llvm_sys::LLVMIntPredicate;
use llvm_sys::core::*;
use llvm_sys::{LLVMModule, LLVMBuilder};
use llvm_sys::prelude::*;
use llvm_sys::analysis::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use std::os::raw::c_char;
use llvm_sys::bit_reader::*;
use llvm_sys::execution_engine::*;

use std::os::raw::{c_ulonglong, c_uint};
use std::ffi::{CString, CStr};
use std::str;
use std::string::String;

use value::{Value, Number};

use libc;


fn char_ptr_to_string(char_ptr: *const c_char) -> String {
    unsafe {
        CStr::from_ptr(char_ptr as *const _).to_str().expect("expected correct char* to str").to_owned()
    }
}

#[derive(Debug, Copy, Clone)]
enum BalloonTypeTag {
    Integer,
    Boolean,
    Float,
}

fn raw_int_to_balloon_type_tag(raw: u64) -> BalloonTypeTag {
    match raw {
        0 => BalloonTypeTag::Integer,
        1 => BalloonTypeTag::Float,
        2 => BalloonTypeTag::Boolean,
        _ => panic!("unknown raw int to type tag: |{}|", raw),
    }
}

/// A struct that keeps ownership of all the strings we've passed to
/// the LLVM API until we destroy the `LLVMModule`.
pub struct Module {
    module: *mut LLVMModule,
    strings: Vec<CString>,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_cstring().to_string_lossy().into_owned())
    }
}

impl Module {
    /// Create a new CString associated with this LLVMModule,
    /// and return a pointer that can be passed to LLVM APIs.
    /// Assumes s is pure-ASCII.
    fn new_string_ptr(&mut self, s: &str) -> *const i8 {
        self.new_mut_string_ptr(s)
    }

    // TODO: ideally our pointers wouldn't be mutable.
    fn new_mut_string_ptr(&mut self, s: &str) -> *mut i8 {
        let cstring = CString::new(s).unwrap();
        let ptr = cstring.as_ptr() as *mut _;
        self.strings.push(cstring);
        ptr
    }

    pub fn to_cstring(&self) -> CString {
        unsafe {
            // LLVM gives us a *char pointer, so wrap it in a CStr to mark it
            // as borrowed.
            let llvm_ir_ptr = LLVMPrintModuleToString(self.module);
            let llvm_ir = CStr::from_ptr(llvm_ir_ptr as *const _);

            // Make an owned copy of the string in our memory space.
            let module_string = CString::new(llvm_ir.to_bytes()).unwrap();

            // Cleanup borrowed string.
            LLVMDisposeMessage(llvm_ir_ptr);

            module_string
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        // Rust requires that drop() is a safe function.
        unsafe {
            LLVMDisposeModule(self.module);
        }
    }
}

/// Wraps LLVM's builder class to provide a nicer API and ensure we
/// always dispose correctly.
struct Builder {
    builder: *mut LLVMBuilder,
}

impl Builder {
    /// Create a new Builder in LLVM's global context.
    fn new() -> Self {
        unsafe { Builder { builder: LLVMCreateBuilder() } }
    }

    fn position_at_end(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        // Rust requires that drop() is a safe function.
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}


// Convert this integer to LLVM's representation of a constant
//integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
const LLVM_FALSE: LLVMBool = 0;

fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}
fn int64_type() -> LLVMTypeRef {
    unsafe { LLVMInt64Type() }
}

fn int1_type() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

fn float64_type() -> LLVMTypeRef {
    unsafe { LLVMDoubleType() }
}

fn tag_type() -> LLVMTypeRef {
    int32_type()
}
fn box_type() -> LLVMTypeRef {

    let mut elem_types = [tag_type(), int64_type()];
    let packed = false;
    unsafe {
        LLVMStructType(elem_types.as_mut_ptr(),
                       elem_types.len() as u32,
                       packed as i32)
    }
}

fn gen_function(module: &mut Module,
                fn_name: &str,
                args: &mut [LLVMTypeRef],
                ret_type: LLVMTypeRef)
                -> LLVMValueRef {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_FALSE);
        let llvmfn = LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type);
        LLVMSetFunctionCallConv(llvmfn, 0);
        llvmfn
    }
}

unsafe fn gen_function_call(module: &mut Module,
                            bb: LLVMBasicBlockRef,
                            fn_name: &str,
                            args: &mut [LLVMValueRef],
                            name: &str)
                            -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);

    let function = LLVMGetNamedFunction(module.module, module.new_string_ptr(fn_name));

    LLVMBuildCall(builder.builder,
                  function,
                  args.as_mut_ptr(),
                  args.len() as c_uint,
                  module.new_string_ptr(name))
}

pub struct LLVMInterpreter {
    pub root_env: Rc<RefCell<Environment>>,
}


impl LLVMInterpreter {
    pub fn new() -> LLVMInterpreter {
        LLVMInterpreter { root_env: Environment::new_root() }
    }
}

fn compile_literal(mut module: &mut Module,
                   bb: LLVMBasicBlockRef,
                   prelude_functions: &PreludeFunctions,
                   literal: &Literal)
                   -> LLVMValueRef {
    unsafe {
        match *literal {
            Literal::Integer(i64val) => {
                println!("compiling int: {}", i64val);
                let builder = Builder::new();
                builder.position_at_end(bb);
                let val = LLVMConstInt(int64_type(), i64val as c_ulonglong, 1);
                println!("created const int; ");
                LLVMBuildCall(builder.builder,
                              prelude_functions.box_i64,
                              [val].as_mut_ptr(),
                              1,
                              module.new_string_ptr("valp"))
               
            }
            Literal::Float(f64val) => {
                let builder = Builder::new();
                builder.position_at_end(bb);
                let val = LLVMConstReal(float64_type(), f64val);
                LLVMBuildCall(builder.builder,
                              prelude_functions.box_f64,
                              [val].as_mut_ptr(),
                              1,
                              module.new_string_ptr("valp"))
            }
            Literal::Bool(boolval) => LLVMConstInt(int1_type(), boolval as c_ulonglong, 1),
            ref other => panic!("unknown literal expr: {:?}", other),
        }
    }
}

struct PreludeFunctions {
    add_box_box: LLVMValueRef,
    box_i64: LLVMValueRef,
    unbox_i64: LLVMValueRef,
    box_f64: LLVMValueRef,
    unbox_f64: LLVMValueRef,
    unbox_tag: LLVMValueRef
}

fn compile_expr(module: &mut Module,
                bb: LLVMBasicBlockRef,
                prelude_functions: &PreludeFunctions,
                expr: &Expr)
                -> LLVMValueRef {
    match *expr {
        Expr::Literal(ref literal) => {
            compile_literal(module, bb, prelude_functions, &literal.data)
        }
        Expr::Binary(ref leftexpr, ref op, ref rightexpr) => unsafe {
            let builder = Builder::new();
            builder.position_at_end(bb);

            let leftbox = compile_expr(module,
                                       bb,
                                       prelude_functions,
                                       &leftexpr.data);
            let rightbox = compile_expr(module,
                                        bb,
                                        prelude_functions,
                                        &rightexpr.data);

            match *op {
                BinOp::Add => {
                    LLVMBuildCall(builder.builder,
                                  prelude_functions.add_box_box,
                                  [leftbox, rightbox].as_mut_ptr(),
                                  2,
                                  module.new_string_ptr("sum"))
                }
                _ => panic!("unimplemented expr operator: {}", op),
            }


        },
        ref other => panic!("unknown compile_expr: {:?}", other),

    }
}

fn compile_statement(mut module: &mut Module,
                     bb: LLVMBasicBlockRef,
                     prelude_functions: &PreludeFunctions,
                     statement: &Stmt)
                     -> LLVMValueRef {
    match *statement {
        Stmt::Expr(ref expr) => {
            compile_expr(&mut module,
                         bb,
                         prelude_functions,
                         &expr.data)
        }
        ref other => panic!("unknown compile: {:?}", other),
    }
}

pub fn get_default_target_triple() -> CString {
    let target_triple;
    unsafe {
        let target_triple_ptr = LLVMGetDefaultTargetTriple();
        target_triple = CStr::from_ptr(target_triple_ptr as *const _).to_owned();
        LLVMDisposeMessage(target_triple_ptr);
    }

    target_triple
}

fn module_set_target_triple(module: &mut Module, target_triple: Option<String>) {

    let target_triple_cstring = if let Some(target_triple) = target_triple {
        CString::new(target_triple).unwrap()
    } else {
        get_default_target_triple()
    };

    // This is necessary for maximum LLVM performance, see
    // http://llvm.org/docs/Frontend/PerformanceTips.html
    unsafe {
        LLVMSetTarget(module.module, target_triple_cstring.as_ptr() as *const _);
    }
}


fn load_llvm_prelude_from_file(path: &str) -> (Module, PreludeFunctions) {
    unsafe  {
        let mut out : LLVMModuleRef = mem::uninitialized();

        let mut out_message_raw : *mut c_char = mem::uninitialized();
        let path_cstr : CString = CString::new(path).unwrap();
        let mut membuf : LLVMMemoryBufferRef = mem::uninitialized();
        if 0 != LLVMCreateMemoryBufferWithContentsOfFile(path_cstr.as_ptr(), &mut membuf, &mut out_message_raw) {
            panic!("unable to read memory buffer from file: {}", char_ptr_to_string(out_message_raw))
        }


        if 0 != LLVMParseBitcode2(membuf, &mut out) {
            panic!("unable to load module")
        }
        let mut module = Module {module: out, strings: Vec::new()};

        let prelude = PreludeFunctions {
            add_box_box:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_add_box_box")),
            box_i64:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_box_i64")),
            unbox_i64:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_unbox_i64")),

            box_f64:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_box_f64")),
            unbox_f64:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_unbox_f64")),

            unbox_tag:  LLVMGetNamedFunction(module.module, module.new_string_ptr("balloon_unbox_tag")),
      
        };

        // TODO: find a nicer way to do this.
        assert!(!prelude.add_box_box.is_null());

        assert!(!prelude.box_i64.is_null());
        assert!(!prelude.unbox_i64.is_null());

        assert!(!prelude.box_f64.is_null());
        assert!(!prelude.unbox_f64.is_null());

        assert!(!prelude.unbox_tag.is_null());

        (module, prelude)
            
    }
}

fn interpret_statements(stmts: &[StmtNode],
                        _: Rc<RefCell<Environment>>)
                        -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let target_triple: Option<String> = None;

    // start with prelude loaded
    let (mut module, prelude_functions) = load_llvm_prelude_from_file("lib/prelude.bc");
    
    module_set_target_triple(&mut module, target_triple);

    println!("set target triple");
    unsafe {

        let main_fn = gen_function(&mut module, "main", &mut [], LLVMPointerType(box_type(), 0));
        LLVMSetFunctionCallConv(main_fn, 0);


        println!("added main...");
        let bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("init"));
        let builder = Builder::new();
        builder.position_at_end(bb);

        let final_value = {
            let mut box_size = LLVMSizeOf(box_type());
            box_size = LLVMConstTruncOrBitCast(box_size, int32_type());
            let mut malloc_args = [box_size];
            let raw_mem =
                gen_function_call(&mut module, bb, "malloc", &mut malloc_args, "final_rawmem");
            LLVMBuildPointerCast(builder.builder,
                                 raw_mem,
                                 LLVMPointerType(box_type(), 0),
                                 module.new_string_ptr("final"))
        };

        println!("added final value.");
        println!("module:\n{:?}\n======\n", module);
        println!("stmts: {:?}", stmts);


        for stmt in stmts.iter() {
            print!("compiling: {:?}", stmt);
            let stmt_valp = compile_statement(&mut module,
                                              bb,
                                              &prelude_functions,
                                              &stmt.data);
            println!("compiled.");
            let box_val = LLVMBuildLoad(builder.builder,
                                        stmt_valp,
                                        module.new_string_ptr("stmt_val"));
            LLVMBuildStore(builder.builder, box_val, final_value);
            println!("@@@@Module after stmt:{:?}\n{:?}\n----", stmt, module);

        }

        LLVMBuildRet(builder.builder, final_value);

        let mut jit = LLVMJIT {};
        jit.add_module(module, main_fn);

    };



    Result::Ok(None)
}

struct LLVMJIT {
    //engine: LLVMExecutionEngineRef
}

#[derive(Debug, Copy, Clone)]
//#[repr(C)]
#[repr(C)]
struct LLVMBoxedStructRepr {
    tag: i32,
    raw: i64,
}

fn llvm_boxed_struct_repr_to_value(llvm_repr: LLVMBoxedStructRepr) -> Value {
    match raw_int_to_balloon_type_tag(llvm_repr.tag as u64) {
        BalloonTypeTag::Integer => Value::Number(Number::Integer(llvm_repr.raw)),
        BalloonTypeTag::Float => unsafe {
            Value::Number(Number::Float(mem::transmute(llvm_repr.raw)))
        },
        _ => panic!("unknown Value"),
    }
}

impl LLVMJIT {
    fn add_c_mappings(engine: *mut LLVMExecutionEngineRef, module: &mut Module) {
        unsafe {
             println!("adding malloc..");
            let malloc = LLVMGetNamedFunction(module.module, module.new_string_ptr("malloc"));
            LLVMAddGlobalMapping(*engine,
                                 malloc,
                                 libc::malloc as usize as *mut libc::c_void);

        }
    }

    fn add_module(&mut self,
                  mut module: Module,
                  mainfn: LLVMValueRef) {
        unsafe {
            let mut error_c_string: *mut i8 = mem::uninitialized();
            LLVMVerifyModule(module.module,
                             LLVMVerifierFailureAction::LLVMAbortProcessAction,
                             &mut error_c_string);
            let err = CStr::from_ptr(error_c_string)
                .to_string_lossy()
                .into_owned();
            println!("@@@@@@ ERROR: {}", err);

            let engine: *mut LLVMExecutionEngineRef = mem::uninitialized();

            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();


            // This does not work on MAC OS? WTF
            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            println!("@@@@@ Creating Execution Engine...");
            if LLVMCreateExecutionEngineForModule(engine, module.module, &mut error_c_string) != 0 {
                panic!("unable to create execution engine.")
            }

            LLVMJIT::add_c_mappings(engine, &mut module);
            println!("@@@@@ Execution Engine Created.");
            LLVMRunFunction(*engine, mainfn, 0, [].as_mut_ptr());
            println!("@@@@@ LLVMRunFunction succeesed");




            let mainfn: fn() -> *const LLVMBoxedStructRepr =
                mem::transmute(LLVMGetFunctionAddress(*engine, module.new_string_ptr("main")));
            let result: *const LLVMBoxedStructRepr = mainfn();
            println!("@@@@ Result of Function call:");
            println!("result ptr : |{:?}|", result);
            println!("*(result ptr) : |{:?}|", *result);

            let result_val = llvm_boxed_struct_repr_to_value(*result);
            println!("Result as value: |{:?}", result_val);
            return;

        }

    }
}

fn interpret_program(program: &[StmtNode],
                     env: Rc<RefCell<Environment>>)
                     -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let result = interpret_statements(program, env.clone())?;
    Ok(result)
}

impl Interpreter for LLVMInterpreter {
    fn run_ast_as_statements(&mut self,
                             statements: &[StmtNode])
                             -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_statements(statements, self.root_env.clone())
    }

    fn run_ast_as_program(&mut self,
                          program: &[StmtNode])
                          -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_program(program, self.root_env.clone())
    }
}
