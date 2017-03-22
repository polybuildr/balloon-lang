use value::*;

pub enum Function {
    Void(fn (args: Vec<Value>)),
    Returning(fn (args: Vec<Value>) -> Value),
}

pub fn builtin_println(args: Vec<Value>) {
    if args.len() == 0 {
        return;
    }
    if args.len() == 1 {
        println!("{}", args.get(0).unwrap());
    } else {
        print!("{}", args.get(0).unwrap());
        for arg in args.iter().skip(1) {
            print!(" {}", arg);
        }
        println!("");
    }
}
