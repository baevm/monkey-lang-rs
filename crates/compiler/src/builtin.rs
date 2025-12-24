use std::rc::Rc;

use monke_core::{
    object::{Array, Builtin, BuiltinFnError, Inspect, Integer, InternalError, Null, Object},
    output::print_output,
};

pub trait BuiltinFunction {
    fn get_by_identifier(ident: &str) -> Option<Object>;

    fn length(args: &[Object]) -> Object;

    fn first(args: &[Object]) -> Object;

    fn last(args: &[Object]) -> Object;

    fn push(args: &[Object]) -> Object;

    fn print(args: &[Object]) -> Object;
}

impl BuiltinFunction for Builtin {
    fn get_by_identifier(ident: &str) -> Option<Object> {
        match ident {
            "len" => Some(Object::Builtin(Builtin {
                func: Rc::new(Self::length),
            })),
            "first" => Some(Object::Builtin(Builtin {
                func: Rc::new(Self::first),
            })),
            "last" => Some(Object::Builtin(Builtin {
                func: Rc::new(Self::last),
            })),
            "push" => Some(Object::Builtin(Builtin {
                func: Rc::new(Self::push),
            })),
            "print" => Some(Object::Builtin(Builtin {
                func: Rc::new(Self::print),
            })),
            _ => None,
        }
    }

    fn length(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::InternalError(InternalError {
                message: BuiltinFnError::WrongNumberOfArgs(format!("Got: {}, want: 1", args.len())),
            });
        }

        match &args[0] {
            Object::String(str_obj) => Object::Integer(Integer {
                value: str_obj.value.len() as i64,
            }),
            Object::Array(arr) => Object::Integer(Integer {
                value: arr.elements.len() as i64,
            }),
            _ => Object::InternalError(InternalError {
                message: BuiltinFnError::WrongNumberOfArgs(format!("Got: {}, want: 0", args[0])),
            }),
        }
    }

    fn first(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::InternalError(InternalError {
                message: BuiltinFnError::WrongNumberOfArgs(format!("Got: {}, want: 1", args.len())),
            });
        }

        if let Object::Array(arr) = &args[0]
            && !arr.elements.is_empty()
        {
            return arr.elements[0].clone();
        }

        Object::Null(Null {})
    }

    fn last(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::InternalError(InternalError {
                message: BuiltinFnError::WrongNumberOfArgs(format!("Got: {}, want: 1", args.len())),
            });
        }

        if let Object::Array(arr) = &args[0]
            && !arr.elements.is_empty()
        {
            return arr.elements.last().unwrap().clone();
        }

        Object::Null(Null {})
    }

    fn push(args: &[Object]) -> Object {
        if args.len() != 2 {
            return Object::InternalError(InternalError {
                message: BuiltinFnError::WrongNumberOfArgs(format!("Got: {}, want: 2", args.len())),
            });
        }

        if let Object::Array(arr) = &args[0] {
            let mut new_elements = arr.elements.clone();
            new_elements.push(args[1].clone());
            return Object::Array(Array {
                elements: new_elements,
            });
        }

        Object::Null(Null {})
    }

    fn print(args: &[Object]) -> Object {
        for arg in args {
            print_output(&arg.inspect());
        }

        Object::Null(Null {})
    }
}
