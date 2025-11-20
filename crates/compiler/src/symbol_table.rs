use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: i64,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: i64,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: SymbolScope::Global,
            index: self.num_definitions,
        };

        self.store.insert(name.to_string(), symbol.clone());

        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::symbol_table::{Symbol, SymbolScope, SymbolTable};

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            (
                "a",
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(&a, expected.get("a").unwrap());

        let b = global.define("b");
        assert_eq!(&b, expected.get("b").unwrap());
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = Vec::from([
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ]);

        for symbol in expected {
            let result = global
                .resolve(&symbol.name)
                .expect("name is not resolvable");

            assert_eq!(&symbol, result);
        }
    }
}
