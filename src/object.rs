use strum_macros::{Display, VariantNames};

pub trait ObjectTrait {
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Eq, Display, VariantNames)]
pub enum Object {
    Integer(Box<Integer>),
    Boolean(Box<Boolean>),
    Null(Box<Null>),
    Return(Box<Return>),
    InternalError(Box<InternalError>),
}

impl ObjectTrait for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(integer_object) => integer_object.inspect(),
            Object::Boolean(boolean_object) => boolean_object.inspect(),
            Object::Null(null_object) => null_object.inspect(),
            Object::Return(return_obj) => return_obj.inspect(),
            Object::InternalError(internal_err) => internal_err.inspect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Integer {
    pub value: i64,
}

impl ObjectTrait for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectTrait for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Null {}

impl ObjectTrait for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Return {
    pub value: Object,
}

impl ObjectTrait for Return {
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InternalError {
    pub message: String,
}

impl ObjectTrait for InternalError {
    fn inspect(&self) -> String {
        format!("ERROR: {:?}", self.message)
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
}
