use value::*;

pub trait Callable {
    fn call(&self, Vec<Value>) -> Option<Value>;
}

pub struct PrintLn {}

impl Callable for PrintLn {
    fn call(&self, args: Vec<Value>) -> Option<Value> {
        if args.len() == 0 {
            return Option::None;
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
        Option::None
    }
}
