use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    pub scope: SymbolScope,
    pub index: usize,
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
    pub num_definitions: usize,

    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: vec![],
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

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let mut obj = self.store.get(name).cloned();

        if obj.is_none() && self.outer.is_some() {
            obj = self.outer.as_mut().unwrap().resolve(name);

            if obj.is_none() {
                return obj;
            }

            let obj = obj.unwrap();

            if matches!(obj.scope, SymbolScope::Global | SymbolScope::Builtin) {
                return Some(obj);
            }

            let free = self.define_free(obj.clone());
            return free;
        }

        obj
    }

    fn define_free(&mut self, original: Symbol) -> Option<Symbol> {
        self.free_symbols.push(original.clone());

        let symbol = Symbol {
            name: original.name.clone(),
            index: self.free_symbols.len() - 1,
            scope: SymbolScope::Free,
        };
        self.store.insert(original.name.clone(), symbol.clone());
        Some(symbol)
    }

    fn define_builtin(&mut self, index: usize, name: &str) -> Option<&Symbol> {
        let symbol = Symbol {
            name: name.to_string(),
            index,
            scope: SymbolScope::Builtin,
        };
        self.store.insert(name.to_string(), symbol);
        self.store.get(name)
    }

    pub fn define_builtins(&mut self) {
        let builtins = ["len", "first", "last", "push", "print"];
        for (i, name) in builtins.iter().enumerate() {
            self.define_builtin(i, name);
        }
    }

    pub fn get_builtin(index: usize) -> Option<&'static str> {
        // gets by index of builtins array in define_builtins
        match index {
            0 => Some("len"),
            1 => Some("first"),
            2 => Some("last"),
            3 => Some("push"),
            4 => Some("print"),
            _ => None,
        }
    }

    pub fn define_function_name(&mut self, name: &str) -> Option<Symbol> {
        let symbol = Symbol {
            name: name.to_string(),
            index: 0,
            scope: SymbolScope::Function,
        };

        self.store.insert(name.to_string(), symbol)
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

            assert_eq!(symbol, result);
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

            assert_eq!(symbol, result);
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

        for (mut symbol_table, expected) in tests {
            for symbol in expected {
                let result = symbol_table
                    .resolve(&symbol.name)
                    .expect("name is not resolvable");

                assert_eq!(symbol, result);
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let expected = vec![
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Builtin,
                index: 0,
            },
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Builtin,
                index: 1,
            },
            Symbol {
                name: "e".to_string(),
                scope: SymbolScope::Builtin,
                index: 2,
            },
            Symbol {
                name: "f".to_string(),
                scope: SymbolScope::Builtin,
                index: 3,
            },
        ];

        let mut global = SymbolTable::new();

        for (i, exp) in expected.iter().enumerate() {
            global.define_builtin(i, &exp.name);
        }

        let first_local = SymbolTable::new_enclosed(global.clone());
        let second_local = SymbolTable::new_enclosed(first_local.clone());

        for mut table in [global, first_local, second_local] {
            for sym in &expected {
                let result = table
                    .resolve(&sym.name)
                    .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));

                assert_eq!(
                    *sym, result,
                    "expected {} to resolve to {:?}, got={:?}",
                    sym.name, sym, result
                );
            }
        }
    }

    #[test]
    fn test_resolve_free() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        let tests = Vec::from([
            (
                first_local,
                vec![
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
                ],
                vec![],
            ),
            (
                second_local,
                vec![
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
                        scope: SymbolScope::Free,
                        index: 0,
                    },
                    Symbol {
                        name: "d".to_string(),
                        scope: SymbolScope::Free,
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
                ],
                vec![
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
                ],
            ),
        ]);

        for mut test in tests {
            for sym in test.1 {
                let result = test
                    .0
                    .resolve(&sym.name)
                    .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));

                assert_eq!(
                    result, sym,
                    "expected: {} to resolve to: {:?}, got: {:?}",
                    sym.name, sym, result
                );
            }

            assert_eq!(test.0.free_symbols.len(), test.2.len());

            for (i, expected_sym) in test.2.iter().enumerate() {
                let result = &test.0.free_symbols[i];

                assert_eq!(*result, expected_sym.clone());
            }
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let mut global = SymbolTable::new();
        global.define("a");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e");
        second_local.define("f");

        let expected = vec![
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Free,
                index: 0,
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
        ];

        for sym in expected {
            let result = second_local
                .resolve(&sym.name)
                .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));

            assert_eq!(
                result, sym,
                "expected {} to resolve to {:?}, got={:?}",
                sym.name, sym, result
            );
        }

        let expected_unresolvable = vec!["b", "d"];

        for name in expected_unresolvable {
            let result = second_local.resolve(name);
            assert!(
                result.is_none(),
                "name {} resolved, but was expected not to",
                name
            );
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");

        let expected = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::Function,
            index: 0,
        };

        let result = global
            .resolve(&expected.name)
            .expect("name is not resolvable");

        assert_eq!(expected, result);
    }

    #[test]
    fn test_shadowing_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");
        global.define("a");

        let expected = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::Global,
            index: 0,
        };

        let result = global
            .resolve(&expected.name)
            .expect("name is not resolvable");

        assert_eq!(expected, result);
    }
}
