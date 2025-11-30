use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: i64,
}

impl Symbol {
    pub fn is_global_scope(&self) -> bool {
        self.scope == SymbolScope::Global
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    num_definitions: i64,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> SymbolTable {
        let mut table = SymbolTable::new();
        table.outer = Some(Box::new(outer));
        table
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let scope = if self.outer.is_none() {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };

        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            scope,
        };

        self.store.insert(name.to_string(), symbol.clone());

        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        let mut obj = self.store.get(name);

        if obj.is_none() && self.outer.is_some() {
            obj = self.outer.as_ref().unwrap().resolve(name);
            return obj;
        }

        obj
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
            (
                "c",
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
            (
                "e",
                Symbol {
                    name: "e".to_string(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "f",
                Symbol {
                    name: "f".to_string(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(&a, expected.get("a").unwrap());

        let b = global.define("b");
        assert_eq!(&b, expected.get("b").unwrap());

        let mut first_local = SymbolTable::new_enclosed(global);

        let c = first_local.define("c");
        assert_eq!(&c, expected.get("c").unwrap());

        let d = first_local.define("d");
        assert_eq!(&d, expected.get("d").unwrap());

        let mut second_local = SymbolTable::new_enclosed(first_local);

        let e = second_local.define("e");
        assert_eq!(&e, expected.get("e").unwrap());

        let f = second_local.define("f");
        assert_eq!(&f, expected.get("f").unwrap());
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

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(global);

        local.define("c");
        local.define("d");

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
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "d".to_string(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ]);

        for symbol in expected {
            let result = local.resolve(&symbol.name).expect("name is not resolvable");

            assert_eq!(&symbol, result);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);

        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());

        second_local.define("e");
        second_local.define("f");

        let tests = vec![
            (
                first_local,
                Vec::from([
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
                    Symbol {
                        name: "c".to_string(),
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "d".to_string(),
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ]),
            ),
            (
                second_local,
                Vec::from([
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
                    Symbol {
                        name: "e".to_string(),
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "f".to_string(),
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ]),
            ),
        ];

        for (symbol_table, expected) in tests {
            for symbol in expected {
                let result = symbol_table
                    .resolve(&symbol.name)
                    .expect("name is not resolvable");

                assert_eq!(&symbol, result);
            }
        }
    }
}
