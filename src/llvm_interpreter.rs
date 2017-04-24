// sample LLVM code lifted from
// https://github.com/Wilfred/bfc/blob/master/src/llvm.rs
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::mem;

use ast::*;
use environment::Environment;
use runtime::*;


use llvm_sys::core::*;
use llvm_sys::{LLVMModule, LLVMBuilder};
use llvm_sys::prelude::*;
use llvm_sys::analysis::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::execution_engine::*;

use std::os::raw::{c_ulonglong, c_uint};
use std::ffi::{CString, CStr};
use std::str;

use libc;


const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

#[derive(Debug, Copy, Clone)]
enum BalloonTypeTag {
    Integer,
    Boolean,
    Float,
}

fn balloon_type_tag_to_int(tag: BalloonTypeTag) -> u64 {
    match tag {
        BalloonTypeTag::Integer => 30,
        BalloonTypeTag::Float => 1,
        BalloonTypeTag::Boolean => 2,
    }
}

#[allow(dead_code)]
fn raw_int_to_balloon_type_tag(raw: u64) -> BalloonTypeTag {
    match raw {
        0 => BalloonTypeTag::Integer,
        1 => BalloonTypeTag::Float,
        2 => BalloonTypeTag::Boolean,
        _ => panic!("unknown raw int to type tag: |{}|", raw),
    }
}

fn balloon_type_tag_to_str<'a>(tag: BalloonTypeTag) -> &'a str {
    match tag {
        BalloonTypeTag::Integer => return "balloon_int",
        BalloonTypeTag::Float => return "balloon_float",
        BalloonTypeTag::Boolean => return "balloon_boolean",
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


/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
fn int32(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val, LLVM_FALSE) }
}

#[allow(dead_code)]
fn int64(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt64Type(), val, LLVM_FALSE) }
}



fn int8_type() -> LLVMTypeRef {
    unsafe { LLVMInt8Type() }
}

fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}
fn int64_type() -> LLVMTypeRef {
    unsafe { LLVMInt64Type() }
}

fn int1_type() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

fn int8_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMInt8Type(), 0) }
}

#[allow(dead_code)]
fn void_type() -> LLVMTypeRef {
    unsafe { LLVMVoidType() }
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
        let struct_ty = LLVMStructType(elem_types.as_mut_ptr(),
                                       elem_types.len() as u32,
                                       packed as i32);
        struct_ty
    }
}

fn add_function(module: &mut Module,
                fn_name: &str,
                args: &mut [LLVMTypeRef],
                ret_type: LLVMTypeRef)
                -> LLVMValueRef {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_FALSE);
        let llvmfn = LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type);
        LLVMSetFunctionCallConv(llvmfn, 0);
        return llvmfn;
    }
}

struct CDeclarations {
    malloc: LLVMValueRef,
    #[allow(dead_code)]
    free: LLVMValueRef,
}

fn add_c_declarations(module: &mut Module) -> CDeclarations {
    let void;
    unsafe {
        void = LLVMVoidType();
    }

    add_function(module,
                 "llvm.memset.p0i8.i32",
                 &mut [int8_ptr_type(), int8_type(), int32_type(), int32_type(), int1_type()],
                 void);

    let malloc = add_function(module, "malloc", &mut [int32_type()], int8_ptr_type());

    let free = add_function(module, "free", &mut [int8_ptr_type()], void);

    add_function(module,
                 "write",
                 &mut [int32_type(), int8_ptr_type(), int32_type()],
                 int32_type());

    add_function(module, "putchar", &mut [int32_type()], int32_type());

    add_function(module, "getchar", &mut [], int32_type());

    CDeclarations {
        malloc: malloc,
        free: free,
    }
}

unsafe fn add_function_call(module: &mut Module,
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

struct BoxUnboxFunctions {
    box_i64: LLVMValueRef,
    unbox_i64: LLVMValueRef,
}


fn cast_i64_to_i64(_: &mut Module, _: &mut Builder, input_val: LLVMValueRef) -> LLVMValueRef {
    input_val
}


fn cast_f64_to_i64(module: &mut Module,
                   builder: &mut Builder,
                   input_val: LLVMValueRef)
                   -> LLVMValueRef {
    unsafe {
        LLVMBuildBitCast(builder.builder,
                         input_val,
                         int64_type(),
                         module.new_string_ptr("input_cast_to_slot"))
    }
}


fn cast_ilower_to_i64(module: &mut Module,
                      builder: &mut Builder,
                      input_val: LLVMValueRef)
                      -> LLVMValueRef {
    unsafe {
        LLVMBuildSExt(builder.builder,
                      input_val,
                      int64_type(),
                      module.new_string_ptr("input_cast_to_slot"))
    }
}


fn gen_box_fn_for_type<F>(mut module: &mut Module,
                          type_tag: BalloonTypeTag,
                          input_type: LLVMTypeRef,
                          gen_cast: F)
                          -> LLVMValueRef
    where F: Fn(&mut Module, &mut Builder, LLVMValueRef) -> LLVMValueRef
{
    let mut builder = Builder::new();
    unsafe {
        let fnname = format!("box_{}", balloon_type_tag_to_str(type_tag));
        let boxfn = add_function(module,
                                 &fnname,
                                 &mut [input_type],
                                 LLVMPointerType(box_type(), 0));
        let bb = LLVMAppendBasicBlock(boxfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let input = LLVMGetParam(boxfn, 0);
        LLVMSetValueName(input, module.new_string_ptr("input"));

        let cast_input = gen_cast(&mut module, &mut builder, input);


        let mut box_size = LLVMSizeOf(box_type());
        box_size = LLVMConstTruncOrBitCast(box_size, int32_type());
        let mut malloc_args = [box_size];
        let malloc_box = add_function_call(module, bb, "malloc", &mut malloc_args, "mallocmem");
        //let boxp = LLVMBuildAlloca(builder.builder, box_type(), module.new_string_ptr("box"));
        let boxp = LLVMBuildPointerCast(builder.builder,
                                        malloc_box,
                                        LLVMPointerType(box_type(), 0),
                                        module.new_string_ptr("box"));

        let slotp = LLVMBuildStructGEP(builder.builder, boxp, 1, module.new_string_ptr("slotp"));
        LLVMBuildStore(builder.builder, cast_input, slotp);

        //TODO: make a map of type tag -> LLVM Value
        let tagp = LLVMGetNamedGlobal(module.module,
                                      module.new_string_ptr(balloon_type_tag_to_str(type_tag)));
        let tag = LLVMBuildLoad(builder.builder, tagp, module.new_string_ptr("tagval"));

        let box_tag =
            LLVMBuildStructGEP(builder.builder, boxp, 0, module.new_string_ptr("box_tag"));
        LLVMBuildStore(builder.builder, tag, box_tag);

        LLVMBuildRet(builder.builder, boxp);
        boxfn
    }
}

fn gen_unbox_for_type<F>(mut module: &mut Module,
                         type_tag: BalloonTypeTag,
                         out_type: LLVMTypeRef,
                         gen_cast: F)
                         -> LLVMValueRef
    where F: Fn(&mut Module, &mut Builder, LLVMValueRef) -> LLVMValueRef
{
    let mut builder = Builder::new();
    unsafe {
        let fnname = format!("unbox_{}", balloon_type_tag_to_str(type_tag));
        let unboxfn = add_function(module,
                                   &fnname,
                                   &mut [LLVMPointerType(box_type(), 0)],
                                   out_type);
        let bb = LLVMAppendBasicBlock(unboxfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let boxp = LLVMGetParam(unboxfn, 0);
        LLVMSetValueName(boxp, module.new_string_ptr("boxp"));

        // LLVMBuildRet(builder.builder, int64(10));
        let slotp = LLVMBuildStructGEP(builder.builder, boxp, 1, module.new_string_ptr("slotp"));
        let rawval = LLVMBuildLoad(builder.builder, slotp, module.new_string_ptr("rawval"));


        let castval = gen_cast(&mut module, &mut builder, rawval);
        LLVMBuildRet(builder.builder, castval);
        unboxfn
    }
}
fn add_global_defn_for_tag(module: &mut Module, tag: BalloonTypeTag) -> LLVMValueRef {
    unsafe {
        let name = module.new_string_ptr(balloon_type_tag_to_str(tag));

        let global = LLVMAddGlobal(module.module, tag_type(), name);
        LLVMSetInitializer(global, int32(balloon_type_tag_to_int(tag)));
        LLVMSetGlobalConstant(global, LLVM_TRUE);
        global
    }
}

fn gen_add_fn(mut module: &mut Module, box_unbox_functions: &BoxUnboxFunctions) -> LLVMValueRef {
    let builder = Builder::new();
    unsafe {
        let fnname = format!("balloon_add");
        let addfn = add_function(module,
                                 &fnname,
                                 &mut [LLVMPointerType(box_type(), 0),
                                       LLVMPointerType(box_type(), 0)],
                                 LLVMPointerType(box_type(), 0));
        let bb = LLVMAppendBasicBlock(addfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let i1 = LLVMGetParam(addfn, 0);
        LLVMSetValueName(i1, module.new_string_ptr("box1"));

        let i2 = LLVMGetParam(addfn, 1);
        LLVMSetValueName(i2, module.new_string_ptr("box2"));


        let int1 = LLVMBuildCall(builder.builder,
                                 box_unbox_functions.unbox_i64,
                                 [i1].as_mut_ptr(),
                                 1,
                                 module.new_string_ptr("raw1"));

        let int2 = LLVMBuildCall(builder.builder,
                                 box_unbox_functions.unbox_i64,
                                 [i1].as_mut_ptr(),
                                 1,
                                 module.new_string_ptr("raw2"));

        let sum = LLVMBuildAdd(builder.builder, int1, int2, module.new_string_ptr("sum"));

        let boxed_sum = LLVMBuildCall(builder.builder,
                                      box_unbox_functions.box_i64,
                                      [sum].as_mut_ptr(),
                                      1,
                                      module.new_string_ptr("boxed_sum"));
        LLVMBuildRet(builder.builder, boxed_sum);
        addfn
    }

}
fn add_balloon_prelude(mut module: &mut Module) -> BoxUnboxFunctions {
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Integer);
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Float);
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Boolean);
    println!("@@@@@@@@@@ after adding global definitions, module:\n{:?}",
             module);

    let box_unbox_functions = BoxUnboxFunctions {
        box_i64: gen_box_fn_for_type(&mut module,
                                     BalloonTypeTag::Integer,
                                     int64_type(),
                                     cast_i64_to_i64),
        unbox_i64: gen_unbox_for_type(&mut module,
                                      BalloonTypeTag::Integer,
                                      int64_type(),
                                      cast_i64_to_i64),
    };


    gen_box_fn_for_type(&mut module,
                        BalloonTypeTag::Float,
                        float64_type(),
                        cast_f64_to_i64);
    gen_box_fn_for_type(&mut module,
                        BalloonTypeTag::Boolean,
                        int1_type(),
                        cast_ilower_to_i64);
    println!("@@@@@@@@@@ after adding box definitions, module:\n{:?}",
             module);

    gen_add_fn(&mut module, &box_unbox_functions);
    println!("@@@@@@@@@@ after adding unbox definitions, module:\n{:?}",
             module);

    box_unbox_functions
}




fn compile_literal(mut module: &mut Module,
                   bb: LLVMBasicBlockRef,
                   box_unbox_functions: &BoxUnboxFunctions,
                   literal: &Literal)
                   -> LLVMValueRef {
    unsafe {
        match literal {
            &Literal::Integer(i64val) => {
                let builder = Builder::new();
                builder.position_at_end(bb);
                let i = LLVMConstInt(int64_type(), i64val as c_ulonglong, 1);
                let box_intp = LLVMBuildCall(builder.builder,
                                             box_unbox_functions.box_i64,
                                             [i].as_mut_ptr(),
                                             1,
                                             module.new_string_ptr("valp"));
                box_intp
            }
            &Literal::Float(f64val) => LLVMConstReal(float64_type(), f64val),
            &Literal::Bool(boolval) => LLVMConstInt(int1_type(), boolval as c_ulonglong, 1),
            other => panic!("unknown literal expr: {:?}", other),
        }
    }
}
fn compile_expr(module: &mut Module,
                bb: LLVMBasicBlockRef,
                box_unbox_functions: &BoxUnboxFunctions,
                expr: &Expr)
                -> LLVMValueRef {
    match expr {
        &Expr::Literal(ref literal) => compile_literal(module, bb, box_unbox_functions, literal),
        &Expr::Binary(ref leftexpr, ref op, ref rightexpr) => unsafe {
            let builder = Builder::new();
            builder.position_at_end(bb);

            let leftbox = compile_expr(module, bb, box_unbox_functions, &leftexpr.data);
            let rightbox = compile_expr(module, bb, box_unbox_functions, &rightexpr.data);

            let leftval = LLVMBuildCall(builder.builder,
                                        box_unbox_functions.unbox_i64,
                                        [leftbox].as_mut_ptr(),
                                        1,
                                        module.new_string_ptr("leftval"));
            let rightval = LLVMBuildCall(builder.builder,
                                         box_unbox_functions.unbox_i64,
                                         [rightbox].as_mut_ptr(),
                                         1,
                                         module.new_string_ptr("rightval"));

            let finalval = match op {
                &BinOp::Add => {
                    LLVMBuildNSWAdd(builder.builder,
                                    leftval,
                                    rightval,
                                    module.new_string_ptr("addval"))
                }
                &BinOp::Sub => {
                    LLVMBuildSub(builder.builder,
                                 leftval,
                                 rightval,
                                 module.new_string_ptr("subval"))
                }
                &BinOp::Mul => {
                    LLVMBuildMul(builder.builder,
                                 leftval,
                                 rightval,
                                 module.new_string_ptr("mulval"))
                }
                &BinOp::Div => {
                    LLVMBuildSDiv(builder.builder,
                                  leftval,
                                  rightval,
                                  module.new_string_ptr("divval"))
                }
                _ => panic!("unimplemented expr operator: {}", op),
            };

            let finalbox = LLVMBuildCall(builder.builder,
                                         box_unbox_functions.box_i64,
                                         [finalval].as_mut_ptr(),
                                         1,
                                         module.new_string_ptr("finalbox"));
            finalbox
        },
        other => panic!("unknown compile_expr: {:?}", other),

    }
}

fn compile_statement(mut module: &mut Module,
                     bb: LLVMBasicBlockRef,
                     box_unbox_functions: &BoxUnboxFunctions,
                     statement: &Stmt)
                     -> LLVMValueRef {
    match statement {
        &Stmt::Expr(ref expr) => compile_expr(&mut module, bb, box_unbox_functions, &expr.data),
        other => panic!("unknown compile: {:?}", other),
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

fn create_module(module_name: &str, target_triple: Option<String>) -> Module {
    let c_module_name = CString::new(module_name).unwrap();
    let module_name_char_ptr = c_module_name.to_bytes_with_nul().as_ptr() as *const _;

    let llvm_module;
    unsafe {
        llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);
    }
    let module = Module {
        module: llvm_module,
        strings: vec![c_module_name],
    };

    let target_triple_cstring = if let Some(target_triple) = target_triple {
        CString::new(target_triple).unwrap()
    } else {
        get_default_target_triple()
    };

    // This is necessary for maximum LLVM performance, see
    // http://llvm.org/docs/Frontend/PerformanceTips.html
    unsafe {
        LLVMSetTarget(llvm_module, target_triple_cstring.as_ptr() as *const _);
    }
    // TODO: add a function to the LLVM C API that gives us the
    // data layout from the target machine.
    module
}

fn interpret_statements(stmts: &[StmtNode],
                        _: Rc<RefCell<Environment>>)
                        -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let target_triple: Option<String> = None;
    let mut module = create_module("ModuleName", target_triple);

    let c_declarations = add_c_declarations(&mut module);
    let box_unbox_functions = add_balloon_prelude(&mut module);

    unsafe {

        let main_fn = add_function(&mut module, "main", &mut [], LLVMPointerType(box_type(), 0));
        LLVMSetFunctionCallConv(main_fn, 0);


        let bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("init"));
        let builder = Builder::new();
        builder.position_at_end(bb);

        let final_value = {
            let mut box_size = LLVMSizeOf(box_type());
            box_size = LLVMConstTruncOrBitCast(box_size, int32_type());
            let mut malloc_args = [box_size];
            let raw_mem =
                add_function_call(&mut module, bb, "malloc", &mut malloc_args, "final_rawmem");
            LLVMBuildPointerCast(builder.builder,
                                 raw_mem,
                                 LLVMPointerType(box_type(), 0),
                                 module.new_string_ptr("final"))
        };


        println!("built store");
        for stmt in stmts.iter() {
            let stmt_valp = compile_statement(&mut module, bb, &box_unbox_functions, &stmt.data);
            let box_val = LLVMBuildLoad(builder.builder,
                                        stmt_valp,
                                        module.new_string_ptr("stmt_val"));
            LLVMBuildStore(builder.builder, box_val, final_value);
        }

        LLVMBuildRet(builder.builder, final_value);
        // LLVMBuildRet(builder.builder, int64(42));
        //LLVMBuildRet(builder.builder, final_value);

        print!("@@@@@@ FINAL MODULE:\n");
        print!("{}", module.to_cstring().to_string_lossy().into_owned());

        let mut jit = LLVMJIT {};
        jit.add_module(module, main_fn, c_declarations);

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

impl LLVMJIT {
    fn add_c_mappings(engine: *mut LLVMExecutionEngineRef, c_declarations: CDeclarations) {
        unsafe {
            LLVMAddGlobalMapping(*engine,
                                 c_declarations.malloc,
                                 mem::transmute(libc::malloc as usize));

        }
    }

    fn add_module(&mut self,
                  mut module: Module,
                  mainfn: LLVMValueRef,
                  c_declarations: CDeclarations) {
        unsafe {
            let mut error_c_string: *mut i8 = mem::uninitialized();
            LLVMVerifyModule(module.module,
                             LLVMVerifierFailureAction::LLVMAbortProcessAction,
                             &mut error_c_string);
            let err = CStr::from_ptr(error_c_string).to_string_lossy().into_owned();
            println!("@@@@@@ ERROR: {}", err);

            let engine: *mut LLVMExecutionEngineRef = mem::uninitialized();

            LLVM_InitializeNativeAsmPrinter();
            LLVM_InitializeNativeAsmParser();


            // This does not work on MAC OS? WTF
            LLVMLinkInMCJIT();
            // LLVMLinkInInterpreter();
            LLVM_InitializeNativeTarget();
            println!("@@@@@ Creating Execution Engine...");
            if LLVMCreateExecutionEngineForModule(engine, module.module, &mut error_c_string) != 0 {
                panic!("unable to create execution engine.")
            }

            LLVMJIT::add_c_mappings(engine, c_declarations);
            println!("@@@@@ Execution Engine Created.");
            LLVMRunFunction(*engine, mainfn, 0, [].as_mut_ptr());
            println!("@@@@@ LLVMRunFunction succeesed");




            let mainfn: fn() -> *const LLVMBoxedStructRepr =
                mem::transmute(LLVMGetFunctionAddress(*engine, module.new_string_ptr("main")));
            let result: *const LLVMBoxedStructRepr = mainfn();
            println!("@@@@ Result of Function call:");
            println!("result ptr : |{:?}|", result);
            println!("*(result ptr) : |{:?}|", *result);

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
