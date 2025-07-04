use std::{cell::RefCell, collections::HashMap, rc::Rc};

use strum_macros::{Display, VariantNames};

use crate::ast::{BlockStatement, Identifier, Stringer};

pub trait ObjectTrait {
    fn inspect(&self) -> String;
}

#[derive(Debug, Display, VariantNames, Clone)]
pub enum Object {
    Integer(Box<Integer>),
    Boolean(Box<Boolean>),
    Null(Box<Null>),
    Return(Box<Return>),
    InternalError(Box<InternalError>),
    Function(Box<Function>),
}

impl ObjectTrait for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(integer_object) => integer_object.inspect(),
            Object::Boolean(boolean_object) => boolean_object.inspect(),
            Object::Null(null_object) => null_object.inspect(),
            Object::Return(return_obj) => return_obj.inspect(),
            Object::InternalError(internal_err) => internal_err.inspect(),
            Object::Function(function) => function.inspect(),
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

impl ObjectTrait for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectTrait for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Null {}

impl ObjectTrait for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Object,
}

impl ObjectTrait for Return {
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, Clone)]
pub struct InternalError {
    pub message: String,
}

impl ObjectTrait for InternalError {
    fn inspect(&self) -> String {
        format!("ERROR: {:?}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl ObjectTrait for Function {
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
}
