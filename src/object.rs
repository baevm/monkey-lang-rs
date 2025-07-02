pub trait ObjectTrait {
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub enum Object {
    Integer(Box<Integer>),
    Boolean(Box<Boolean>),
    Null(Box<Null>),
    Return(Box<Return>),
}

impl ObjectTrait for Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(integer_object) => integer_object.inspect(),
            Object::Boolean(boolean_object) => boolean_object.inspect(),
            Object::Null(null_object) => null_object.inspect(),
            Object::Return(return_obj) => return_obj.inspect(),
        }
    }
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl ObjectTrait for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectTrait for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
pub struct Null {}

impl ObjectTrait for Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug)]
pub struct Return {
    pub value: Object,
}

impl ObjectTrait for Return {
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}
