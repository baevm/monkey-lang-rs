use std::{
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use fnv::FnvHasher;
use strum_macros::{Display, VariantNames};

use crate::ast::{BlockStatement, Identifier};

pub trait Inspect {
    fn inspect(&self) -> String;
}

pub trait HashableKey {
    fn hash_key(&self) -> HashKey;
}

#[derive(Debug, Display, VariantNames, Clone)]
pub enum Object {
    Integer(Box<Integer>),
    Boolean(Box<Boolean>),
    Null(Box<Null>),
    Return(Box<Return>),
    InternalError(Box<InternalError>),
    Function(Box<Function>),
    String(Box<StringObj>),
    Builtin(Box<Builtin>),
    Array(Box<Array>),
    HashObj(Box<HashObj>),
    CompiledFunction(Box<CompiledFunction>),
}

impl Inspect for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(integer_object) => integer_object.inspect(),
            Object::Boolean(boolean_object) => boolean_object.inspect(),
            Object::Null(null_object) => null_object.inspect(),
            Object::Return(return_obj) => return_obj.inspect(),
            Object::InternalError(internal_err) => internal_err.inspect(),
            Object::Function(function) => function.inspect(),
            Object::String(string_obj) => string_obj.inspect(),
            Object::Builtin(builtin) => builtin.inspect(),
            Object::Array(array) => array.inspect(),
            Object::HashObj(hash_obj) => hash_obj.inspect(),
            Object::CompiledFunction(compiled_fn) => compiled_fn.inspect(),
        }
    }
}

impl Object {
    pub fn get_hash(&self) -> Option<HashKey> {
        match self {
            Object::Integer(integer) => Some(integer.hash_key()),
            Object::Boolean(boolean) => Some(boolean.hash_key()),
            Object::String(string_obj) => Some(string_obj.hash_key()),
            _ => None,
        }
    }
}

/// Stores bindings of variables
#[derive(Debug, Clone)]
pub struct Environment {
    // TODO: change Object to Rc<Object> to avoid cloning
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(outer);
        env
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        // TODO: !!!! avoid cloning

        if let Some(obj) = self.store.get(name) {
            return Some(obj.clone());
        }

        if let Some(outer) = &self.outer {
            return outer.borrow().get(name);
        }

        None
    }

    pub fn set(&mut self, name: &str, obj: Object) -> Option<Object> {
        self.store.insert(name.to_string(), obj)
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Inspect for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl HashableKey for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey {
            value: self.value.try_into().unwrap_or(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Inspect for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl HashableKey for Boolean {
    fn hash_key(&self) -> HashKey {
        let value = if self.value { 1 } else { 0 };
        HashKey { value }
    }
}

#[derive(Debug, Clone)]
pub struct Null {}

impl Inspect for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Object,
}

impl Inspect for Return {
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, Clone)]
pub enum EvaluateErr {
    UnknownOperator(String),
    TypeError(String),
    IndexNotSupported(String),
    UnknownIdentifier(String),
    UnknownHashKey(String),
    WrongNumberOfArgs(String),
}

impl std::fmt::Display for EvaluateErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluateErr::UnknownOperator(e) => write!(f, "unknown operator: {e}"),
            EvaluateErr::UnknownIdentifier(e) => write!(f, "unknown identifier: {e}"),
            EvaluateErr::TypeError(e) => write!(f, "type error: {e}"),
            EvaluateErr::IndexNotSupported(e) => write!(f, "index operator not supported: {e}"),
            EvaluateErr::UnknownHashKey(e) => write!(f, "unknown as hash key: {e}"),
            EvaluateErr::WrongNumberOfArgs(e) => write!(f, "wrong number of arguments. {e}"),
        }
    }
}

// TODO: move to interpreter crate
#[derive(Debug, Clone)]
pub struct InternalError {
    pub message: EvaluateErr,
}

impl Inspect for InternalError {
    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Inspect for Function {
    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        let mut sb = String::new();

        sb.push_str(&format!(
            "function({}) {{\n {} \n}}",
            &params.join(", "),
            self.body.to_string()
        ));

        sb
    }
}

#[derive(Debug, Clone)]
pub struct StringObj {
    pub value: String,
}

impl Inspect for StringObj {
    fn inspect(&self) -> String {
        self.value.clone()
    }
}

impl HashableKey for StringObj {
    fn hash_key(&self) -> HashKey {
        let mut hasher = FnvHasher::default();
        self.value.hash(&mut hasher);

        HashKey {
            value: hasher.finish(),
        }
    }
}

pub type BuiltinFunc = Rc<dyn Fn(&[Object]) -> Object>;

#[derive(Clone)]
pub struct Builtin {
    pub func: BuiltinFunc,
}

impl Inspect for Builtin {
    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Builtin Function")
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl Inspect for Array {
    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|el| el.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashKey {
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, Clone)]
pub struct HashObj {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl Inspect for HashObj {
    fn inspect(&self) -> String {
        let elements: Vec<String> = self
            .pairs
            .values()
            .map(|pair| format!("{}: {}", pair.key.inspect(), pair.value.inspect()))
            .collect();

        format!("{{{}}}", elements.join(", "))
    }
}

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub instructions: Vec<u8>,
    pub num_locals: i64,
    pub num_parameters: i64,
}

impl Inspect for CompiledFunction {
    fn inspect(&self) -> String {
        format!("CompiledFunction")
    }
}

macro_rules! impl_display_name {
    ($($type:ty => $name:expr),* $(,)?) => {
        $(
            impl std::fmt::Display for $type {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, $name)
                }
            }
        )*
    };
}

impl_display_name! {
    Integer => "Integer",
    Boolean => "Boolean",
    Null => "Null",
    Return => "Return",
    InternalError => "InternalError",
    Function => "Function",
    StringObj => "String",
    Builtin => "Builtin Function",
    Array => "Array",
    HashObj => "Object"
}

#[cfg(test)]
mod tests {
    use crate::object::{HashableKey, StringObj};

    #[test]
    fn test_string_hash_key() {
        let same1 = StringObj {
            value: "Hello world".to_string(),
        };
        let same2 = StringObj {
            value: "Hello world".to_string(),
        };

        assert_eq!(same1.hash_key().value, same2.hash_key().value);

        let diff1 = StringObj {
            value: "Bob".to_string(),
        };
        let diff2 = StringObj {
            value: "Bob".to_string(),
        };

        assert_eq!(diff1.hash_key().value, diff2.hash_key().value);

        assert_ne!(same1.hash_key().value, diff2.hash_key().value);
    }
}
