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
use llvm_sys::execution_engine::*;

use std::os::raw::{c_ulonglong, c_uint};
use std::ffi::{CString, CStr};
use std::str;
use std::string::String;

use value::{Value, Number};

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
        BalloonTypeTag::Integer => 0,
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

fn balloon_type_tag_to_llvm_value(module: &mut Module, tag: BalloonTypeTag) -> LLVMValueRef {
    unsafe {
        LLVMGetNamedGlobal(
            module.module,
            module.new_string_ptr(balloon_type_tag_to_str(tag)),
        )
    }
}

fn balloon_type_tag_to_str<'a>(tag: BalloonTypeTag) -> &'a str {
    match tag {
        BalloonTypeTag::Integer => "balloon_int",
        BalloonTypeTag::Float => "balloon_float",
        BalloonTypeTag::Boolean => "balloon_boolean",
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
        LLVMStructType(
            elem_types.as_mut_ptr(),
            elem_types.len() as u32,
            packed as i32,
        )
    }
}

fn gen_function(
    module: &mut Module,
    fn_name: &str,
    args: &mut [LLVMTypeRef],
    ret_type: LLVMTypeRef,
) -> LLVMValueRef {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_FALSE);
        let llvmfn = LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type);
        LLVMSetFunctionCallConv(llvmfn, 0);
        llvmfn
    }
}

struct CDeclarations {
    malloc: LLVMValueRef,
    #[allow(dead_code)]
    free: LLVMValueRef,
}

fn gen_c_declarations(module: &mut Module) -> CDeclarations {
    let void;
    unsafe {
        void = LLVMVoidType();
    }

    gen_function(
        module,
        "llvm.memset.p0i8.i32",
        &mut [
            int8_ptr_type(),
            int8_type(),
            int32_type(),
            int32_type(),
            int1_type(),
        ],
        void,
    );

    let malloc = gen_function(module, "malloc", &mut [int32_type()], int8_ptr_type());

    let free = gen_function(module, "free", &mut [int8_ptr_type()], void);

    gen_function(
        module,
        "write",
        &mut [int32_type(), int8_ptr_type(), int32_type()],
        int32_type(),
    );

    gen_function(module, "putchar", &mut [int32_type()], int32_type());

    gen_function(module, "getchar", &mut [], int32_type());

    CDeclarations {
        malloc: malloc,
        free: free,
    }
}

unsafe fn gen_function_call(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    fn_name: &str,
    args: &mut [LLVMValueRef],
    name: &str,
) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);

    let function = LLVMGetNamedFunction(module.module, module.new_string_ptr(fn_name));

    LLVMBuildCall(
        builder.builder,
        function,
        args.as_mut_ptr(),
        args.len() as c_uint,
        module.new_string_ptr(name),
    )
}


struct BoxUnboxFunctions {
    box_i64: LLVMValueRef,
    unbox_i64: LLVMValueRef,
    box_f64: LLVMValueRef,
    unbox_f64: LLVMValueRef,
    unbox_tag: LLVMValueRef,
}

pub struct LLVMInterpreter {
    pub root_env: Rc<RefCell<Environment>>,
}


impl LLVMInterpreter {
    pub fn new() -> LLVMInterpreter {
        LLVMInterpreter { root_env: Environment::new_root() }
    }
}



fn cast_i64_to_i64(_: &mut Module, _: &mut Builder, input_val: LLVMValueRef) -> LLVMValueRef {
    input_val
}

fn cast_i64_to_f64(
    module: &mut Module,
    builder: &mut Builder,
    input_val: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        LLVMBuildBitCast(
            builder.builder,
            input_val,
            float64_type(),
            module.new_string_ptr("i64_to_f64"),
        )
    }
}

fn cast_f64_to_i64(
    module: &mut Module,
    builder: &mut Builder,
    input_val: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        LLVMBuildBitCast(
            builder.builder,
            input_val,
            int64_type(),
            module.new_string_ptr("f64_to_i64"),
        )
    }
}


#[allow(dead_code)]
fn cast_ilower_to_i64(
    module: &mut Module,
    builder: &mut Builder,
    input_val: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        LLVMBuildSExt(
            builder.builder,
            input_val,
            int64_type(),
            module.new_string_ptr("ilower_to_i64"),
        )
    }
}

fn gen_unbox_tag(mut module: &mut Module) -> LLVMValueRef {
    let builder = Builder::new();
    unsafe {
        let unboxfn = gen_function(
            module,
            "unbox_tag",
            &mut [LLVMPointerType(box_type(), 0)],
            tag_type(),
        );
        let bb = LLVMAppendBasicBlock(unboxfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let boxp = LLVMGetParam(unboxfn, 0);
        LLVMSetValueName(boxp, module.new_string_ptr("boxp"));

        let tagp = LLVMBuildStructGEP(builder.builder, boxp, 0, module.new_string_ptr("tagp"));
        let tag = LLVMBuildLoad(builder.builder, tagp, module.new_string_ptr("tag"));

        LLVMBuildRet(builder.builder, tag);
        unboxfn
    }
}

fn gen_box_fn_for_type<F>(
    mut module: &mut Module,
    type_tag: BalloonTypeTag,
    input_type: LLVMTypeRef,
    gen_cast: F,
) -> LLVMValueRef
where
    F: Fn(&mut Module, &mut Builder, LLVMValueRef) -> LLVMValueRef,
{
    let mut builder = Builder::new();
    unsafe {
        let fnname = format!("box_{}", balloon_type_tag_to_str(type_tag));
        let boxfn = gen_function(
            module,
            &fnname,
            &mut [input_type],
            LLVMPointerType(box_type(), 0),
        );
        let bb = LLVMAppendBasicBlock(boxfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let input = LLVMGetParam(boxfn, 0);
        LLVMSetValueName(input, module.new_string_ptr("input"));

        let cast_input = gen_cast(&mut module, &mut builder, input);


        let mut box_size = LLVMSizeOf(box_type());
        box_size = LLVMConstTruncOrBitCast(box_size, int32_type());
        let mut malloc_args = [box_size];
        let malloc_box = gen_function_call(module, bb, "malloc", &mut malloc_args, "mallocmem");
        // let boxp = LLVMBuildAlloca(builder.builder, box_type(), module.new_string_ptr("box"));
        let boxp = LLVMBuildPointerCast(
            builder.builder,
            malloc_box,
            LLVMPointerType(box_type(), 0),
            module.new_string_ptr("box"),
        );

        let slotp = LLVMBuildStructGEP(builder.builder, boxp, 1, module.new_string_ptr("slotp"));
        LLVMBuildStore(builder.builder, cast_input, slotp);

        // TODO: make a map of type tag -> LLVM Value
        // let tagp = LLVMGetNamedGlobal(module.module,
        //                              module.new_string_ptr(balloon_type_tag_to_str(type_tag)));
        let tagp = balloon_type_tag_to_llvm_value(module, type_tag);
        let tag = LLVMBuildLoad(builder.builder, tagp, module.new_string_ptr("tagval"));

        let box_tag =
            LLVMBuildStructGEP(builder.builder, boxp, 0, module.new_string_ptr("box_tag"));
        LLVMBuildStore(builder.builder, tag, box_tag);

        LLVMBuildRet(builder.builder, boxp);

        boxfn


    }
}

fn gen_unbox_for_type<F>(
    mut module: &mut Module,
    type_tag: BalloonTypeTag,
    out_type: LLVMTypeRef,
    gen_cast: F,
) -> LLVMValueRef
where
    F: Fn(&mut Module, &mut Builder, LLVMValueRef) -> LLVMValueRef,
{
    let mut builder = Builder::new();
    unsafe {
        let fnname = format!("unbox_{}", balloon_type_tag_to_str(type_tag));
        let unboxfn = gen_function(
            module,
            &fnname,
            &mut [LLVMPointerType(box_type(), 0)],
            out_type,
        );
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

fn gen_add_box_f64_box_f64(
    mut module: &mut Module,
    box_unbox_functions: &BoxUnboxFunctions,
) -> LLVMValueRef {
    let builder = Builder::new();
    unsafe {
        let fnname = "balloon_add_box_f64_box_f64";
        let addfn = gen_function(
            module,
            fnname,
            &mut [
                LLVMPointerType(box_type(), 0),
                LLVMPointerType(box_type(), 0),
            ],
            LLVMPointerType(box_type(), 0),
        );
        let bb = LLVMAppendBasicBlock(addfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let b1 = LLVMGetParam(addfn, 0);
        LLVMSetValueName(b1, module.new_string_ptr("box1"));

        let b2 = LLVMGetParam(addfn, 1);
        LLVMSetValueName(b2, module.new_string_ptr("box2"));


        let float1 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_f64,
            [b1].as_mut_ptr(),
            1,
            module.new_string_ptr("raw1"),
        );

        let float2 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_f64,
            [b2].as_mut_ptr(),
            1,
            module.new_string_ptr("raw2"),
        );

        let sum = LLVMBuildFAdd(
            builder.builder,
            float1,
            float2,
            module.new_string_ptr("sum"),
        );

        let boxed_sum = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.box_f64,
            [sum].as_mut_ptr(),
            1,
            module.new_string_ptr("boxed_sum"),
        );
        LLVMBuildRet(builder.builder, boxed_sum);
        addfn
    }

}

#[allow(dead_code)]
fn gen_add_box_i64_box_f64(
    mut module: &mut Module,
    box_unbox_functions: &BoxUnboxFunctions,
) -> LLVMValueRef {
    let builder = Builder::new();
    unsafe {
        let fnname = "balloon_add_box_i64_box_f64";
        let addfn = gen_function(
            module,
            fnname,
            &mut [
                LLVMPointerType(box_type(), 0),
                LLVMPointerType(box_type(), 0),
            ],
            LLVMPointerType(box_type(), 0),
        );
        let bb = LLVMAppendBasicBlock(addfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let b1 = LLVMGetParam(addfn, 0);
        LLVMSetValueName(b1, module.new_string_ptr("box1"));

        let b2 = LLVMGetParam(addfn, 1);
        LLVMSetValueName(b2, module.new_string_ptr("box2"));


        let int1 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_i64,
            [b1].as_mut_ptr(),
            1,
            module.new_string_ptr("raw1"),
        );

        let float2 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_f64,
            [b2].as_mut_ptr(),
            1,
            module.new_string_ptr("raw2"),
        );

        println!("@@@@ module with int1, float2 load:\n{:?}\n-----\n", module);
        let float1 = LLVMBuildSIToFP(
            builder.builder,
            int1,
            float64_type(),
            module.new_string_ptr("raw1_float"),
        );

        println!("@@@@ module with int1 cast:\n{:?}\n-----\n", module);


        let sum = LLVMBuildFAdd(
            builder.builder,
            float1,
            float2,
            module.new_string_ptr("sum"),
        );

        println!(
            "@@@@ module with int + float sum built:\n{:?}\n-----\n",
            module
        );
        let boxed_sum = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.box_f64,
            [sum].as_mut_ptr(),
            1,
            module.new_string_ptr("boxed_sum"),
        );
        LLVMBuildRet(builder.builder, boxed_sum);
        addfn
    }
}


fn gen_add_box_i64_box_i64(
    mut module: &mut Module,
    box_unbox_functions: &BoxUnboxFunctions,
) -> LLVMValueRef {
    let builder = Builder::new();
    unsafe {
        let fnname = "balloon_add_box_i64_box_i64";
        let addfn = gen_function(
            module,
            fnname,
            &mut [
                LLVMPointerType(box_type(), 0),
                LLVMPointerType(box_type(), 0),
            ],
            LLVMPointerType(box_type(), 0),
        );
        let bb = LLVMAppendBasicBlock(addfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let b1 = LLVMGetParam(addfn, 0);
        LLVMSetValueName(b1, module.new_string_ptr("box1"));

        let b2 = LLVMGetParam(addfn, 1);
        LLVMSetValueName(b2, module.new_string_ptr("box2"));


        let int1 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_i64,
            [b1].as_mut_ptr(),
            1,
            module.new_string_ptr("raw1"),
        );

        let int2 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_i64,
            [b2].as_mut_ptr(),
            1,
            module.new_string_ptr("raw2"),
        );

        let sum = LLVMBuildAdd(builder.builder, int1, int2, module.new_string_ptr("sum"));

        let boxed_sum = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.box_i64,
            [sum].as_mut_ptr(),
            1,
            module.new_string_ptr("boxed_sum"),
        );
        LLVMBuildRet(builder.builder, boxed_sum);
        addfn
    }
}

fn gen_add_box_box(module: &mut Module, box_unbox_functions: &BoxUnboxFunctions) -> LLVMValueRef {

    let add_f64_f64 = gen_add_box_f64_box_f64(module, box_unbox_functions);
    let add_i64_f64 = gen_add_box_i64_box_f64(module, box_unbox_functions);
    let add_i64_i64 = gen_add_box_i64_box_i64(module, box_unbox_functions);

    let builder = Builder::new();
    unsafe {
        let fnname = "balloon_add_box_box";
        let addfn = gen_function(
            module,
            fnname,
            &mut [
                LLVMPointerType(box_type(), 0),
                LLVMPointerType(box_type(), 0),
            ],
            LLVMPointerType(box_type(), 0),
        );
        let bb = LLVMAppendBasicBlock(addfn, module.new_string_ptr("entry"));
        builder.position_at_end(bb);

        let b1 = LLVMGetParam(addfn, 0);
        LLVMSetValueName(b1, module.new_string_ptr("box1"));

        let b2 = LLVMGetParam(addfn, 1);
        LLVMSetValueName(b2, module.new_string_ptr("box2"));

        let builder = Builder::new();
        builder.position_at_end(bb);

        let tag1 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_tag,
            [b1].as_mut_ptr(),
            1,
            module.new_string_ptr("tag1"),
        );
        let tag2 = LLVMBuildCall(
            builder.builder,
            box_unbox_functions.unbox_tag,
            [b2].as_mut_ptr(),
            1,
            module.new_string_ptr("tag2"),
        );



        // FIXME: LLVM does not allow non-constants to be branches in switch-case.
        // Very weird. Will need to variable alias or something?
        // Should not keep BalloonTypeTag as global
        let int_tag = int32(balloon_type_tag_to_int(BalloonTypeTag::Integer) as
            c_ulonglong);
        let float_tag = int32(balloon_type_tag_to_int(BalloonTypeTag::Float) as
            c_ulonglong);

        let bb_err_unknown_add_types = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("unknown_add_types"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);
            // TODO: replace this with some actual error thing
            let error_val = int64((-42 as i64) as c_ulonglong);
            let error_box = LLVMBuildCall(
                builder_inner.builder,
                box_unbox_functions.box_i64,
                [error_val].as_mut_ptr(),
                1,
                module.new_string_ptr("errorbox"),
            );
            LLVMBuildRet(builder_inner.builder, error_box);
            bb_inner
        };

        // float + float
        let bb_float_float = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("float_float"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let sum = LLVMBuildCall(
                builder_inner.builder,
                add_f64_f64,
                [b1, b2].as_mut_ptr(),
                2,
                module.new_string_ptr("sum"),
            );
            LLVMBuildRet(builder_inner.builder, sum);
            bb_inner

        };

        // int + int
        let bb_int_int = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("int_int"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let sum = LLVMBuildCall(
                builder_inner.builder,
                add_i64_i64,
                [b1, b2].as_mut_ptr(),
                2,
                module.new_string_ptr("sum"),
            );
            LLVMBuildRet(builder_inner.builder, sum);
            bb_inner

        };

        // float + int
        let bb_float_int = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("float_int"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let sum = LLVMBuildCall(
                builder_inner.builder,
                add_i64_f64,
                [b2, b1].as_mut_ptr(),
                2,
                module.new_string_ptr("sum"),
            );
            LLVMBuildRet(builder_inner.builder, sum);
            bb_inner
        };

        // int + float
        let bb_int_float = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("int_float"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let sum = LLVMBuildCall(
                builder_inner.builder,
                add_i64_f64,
                [b1, b2].as_mut_ptr(),
                2,
                module.new_string_ptr("sum"),
            );
            LLVMBuildRet(builder_inner.builder, sum);
            bb_inner

        };

        let bb_i64_wildcard = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("int_wildcard"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let switch = LLVMBuildSwitch(builder_inner.builder, tag2, bb_err_unknown_add_types, 2);
            LLVMAddCase(switch, int_tag, bb_int_int);
            LLVMAddCase(switch, float_tag, bb_int_float);
            bb_inner
        };

        let bb_f64_wildcard = {
            let bb_inner = LLVMAppendBasicBlock(addfn, module.new_string_ptr("float_wildcard"));
            let builder_inner = Builder::new();
            builder_inner.position_at_end(bb_inner);

            let switch = LLVMBuildSwitch(builder_inner.builder, tag2, bb_err_unknown_add_types, 2);

            LLVMAddCase(switch, int_tag, bb_float_int);
            LLVMAddCase(switch, float_tag, bb_float_float);
            bb_inner
        };

        let switch_tag1 = LLVMBuildSwitch(builder.builder, tag1, bb_err_unknown_add_types, 2);
        LLVMAddCase(switch_tag1, int_tag, bb_i64_wildcard);
        LLVMAddCase(switch_tag1, float_tag, bb_f64_wildcard);

        addfn
    }
}


fn gen_balloon_prelude(mut module: &mut Module) -> BoxUnboxFunctions {
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Integer);
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Float);
    add_global_defn_for_tag(&mut module, BalloonTypeTag::Boolean);
    println!(
        "@@@@@@@@@@ after adding global definitions, module:\n{:?}",
        module
    );

    let box_unbox_functions = BoxUnboxFunctions {
        box_i64: gen_box_fn_for_type(
            &mut module,
            BalloonTypeTag::Integer,
            int64_type(),
            cast_i64_to_i64,
        ),
        unbox_i64: gen_unbox_for_type(
            &mut module,
            BalloonTypeTag::Integer,
            int64_type(),
            cast_i64_to_i64,
        ),
        box_f64: gen_box_fn_for_type(
            &mut module,
            BalloonTypeTag::Float,
            float64_type(),
            cast_f64_to_i64,
        ),
        unbox_f64: gen_unbox_for_type(
            &mut module,
            BalloonTypeTag::Float,
            float64_type(),
            cast_i64_to_f64,
        ),
        unbox_tag: gen_unbox_tag(&mut module),
    };

    println!(
        "@@@@ module after box_unbox_functions:\n{:?}\n-----",
        module
    );


    box_unbox_functions
}




fn compile_literal(
    mut module: &mut Module,
    bb: LLVMBasicBlockRef,
    box_unbox_functions: &BoxUnboxFunctions,
    literal: &Literal,
) -> LLVMValueRef {
    unsafe {
        match *literal {
            Literal::Integer(i64val) => {
                let builder = Builder::new();
                builder.position_at_end(bb);
                let val = LLVMConstInt(int64_type(), i64val as c_ulonglong, 1);
                LLVMBuildCall(
                    builder.builder,
                    box_unbox_functions.box_i64,
                    [val].as_mut_ptr(),
                    1,
                    module.new_string_ptr("valp"),
                )
            }
            Literal::Float(f64val) => {
                let builder = Builder::new();
                builder.position_at_end(bb);
                let val = LLVMConstReal(float64_type(), f64val);
                LLVMBuildCall(
                    builder.builder,
                    box_unbox_functions.box_f64,
                    [val].as_mut_ptr(),
                    1,
                    module.new_string_ptr("valp"),
                )
            }
            Literal::Bool(boolval) => LLVMConstInt(int1_type(), boolval as c_ulonglong, 1),
            ref other => panic!("unknown literal expr: {:?}", other),
        }
    }
}


struct ArithmeticFunctions {
    add_box_box: LLVMValueRef,
}


fn compile_expr(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    box_unbox_functions: &BoxUnboxFunctions,
    arith_functions: &ArithmeticFunctions,
    expr: &Expr,
) -> LLVMValueRef {
    match *expr {
        Expr::Literal(ref literal) => {
            compile_literal(module, bb, box_unbox_functions, &literal.data)
        }
        Expr::Binary(ref leftexpr, ref op, ref rightexpr) => unsafe {
            let builder = Builder::new();
            builder.position_at_end(bb);

            let leftbox = compile_expr(
                module,
                bb,
                box_unbox_functions,
                arith_functions,
                &leftexpr.data,
            );
            let rightbox = compile_expr(
                module,
                bb,
                box_unbox_functions,
                arith_functions,
                &rightexpr.data,
            );

            print!("@@@@module at compile_expr:\n{:?}\n-----", module);

            match *op {
                BinOp::Add => {
                    LLVMBuildCall(
                        builder.builder,
                        arith_functions.add_box_box,
                        [leftbox, rightbox].as_mut_ptr(),
                        2,
                        module.new_string_ptr("sum"),
                    )
                }
                _ => panic!("unimplemented expr operator: {}", op),
            }


        },
        ref other => panic!("unknown compile_expr: {:?}", other),

    }
}

fn compile_statement(
    mut module: &mut Module,
    bb: LLVMBasicBlockRef,
    box_unbox_functions: &BoxUnboxFunctions,
    arith_functions: &ArithmeticFunctions,
    statement: &Stmt,
) -> LLVMValueRef {
    match *statement {
        Stmt::Expr(ref expr) => {
            compile_expr(
                &mut module,
                bb,
                box_unbox_functions,
                arith_functions,
                &expr.data,
            )
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

fn interpret_statements(
    stmts: &[StmtNode],
    _: Rc<RefCell<Environment>>,
) -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let target_triple: Option<String> = None;
    let mut module = create_module("ModuleName", target_triple);

    let c_declarations = gen_c_declarations(&mut module);
    let box_unbox_functions = gen_balloon_prelude(&mut module);

    let arith_functions =
        ArithmeticFunctions { add_box_box: gen_add_box_box(&mut module, &box_unbox_functions) };

    print!(
        "@@@@@@Module after all prelude generation:\n{:?}\n----",
        module
    );

    unsafe {

        let main_fn = gen_function(&mut module, "main", &mut [], LLVMPointerType(box_type(), 0));
        LLVMSetFunctionCallConv(main_fn, 0);


        let bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("init"));
        let builder = Builder::new();
        builder.position_at_end(bb);

        let final_value = {
            let mut box_size = LLVMSizeOf(box_type());
            box_size = LLVMConstTruncOrBitCast(box_size, int32_type());
            let mut malloc_args = [box_size];
            let raw_mem =
                gen_function_call(&mut module, bb, "malloc", &mut malloc_args, "final_rawmem");
            LLVMBuildPointerCast(
                builder.builder,
                raw_mem,
                LLVMPointerType(box_type(), 0),
                module.new_string_ptr("final"),
            )
        };


        println!("@@@@built store\n{:?}\n----", module);

        for stmt in stmts.iter() {
            let stmt_valp = compile_statement(
                &mut module,
                bb,
                &box_unbox_functions,
                &arith_functions,
                &stmt.data,
            );
            let box_val = LLVMBuildLoad(
                builder.builder,
                stmt_valp,
                module.new_string_ptr("stmt_val"),
            );
            LLVMBuildStore(builder.builder, box_val, final_value);
            println!("@@@@Module after stmt:{:?}\n{:?}\n----", stmt, module);

        }

        LLVMBuildRet(builder.builder, final_value);
        // LLVMBuildRet(builder.builder, int64(42));
        // LLVMBuildRet(builder.builder, final_value);

        println!("@@@@@@ FINAL MODULE:");
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
    fn add_c_mappings(engine: *mut LLVMExecutionEngineRef, c_declarations: CDeclarations) {
        unsafe {
            LLVMAddGlobalMapping(
                *engine,
                c_declarations.malloc,
                libc::malloc as usize as *mut libc::c_void,
            );

        }
    }

    fn add_module(
        &mut self,
        mut module: Module,
        mainfn: LLVMValueRef,
        c_declarations: CDeclarations,
    ) {
        unsafe {
            let mut error_c_string: *mut i8 = mem::uninitialized();
            LLVMVerifyModule(
                module.module,
                LLVMVerifierFailureAction::LLVMAbortProcessAction,
                &mut error_c_string,
            );
            let err = CStr::from_ptr(error_c_string)
                .to_string_lossy()
                .into_owned();
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
                mem::transmute(LLVMGetFunctionAddress(
                    *engine,
                    module.new_string_ptr("main"),
                ));
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

fn interpret_program(
    program: &[StmtNode],
    env: Rc<RefCell<Environment>>,
) -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let result = interpret_statements(program, env.clone())?;
    Ok(result)
}

impl Interpreter for LLVMInterpreter {
    fn run_ast_as_statements(
        &mut self,
        statements: &[StmtNode],
    ) -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_statements(statements, self.root_env.clone())
    }

    fn run_ast_as_program(
        &mut self,
        program: &[StmtNode],
    ) -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_program(program, self.root_env.clone())
    }
}
