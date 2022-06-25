use std::collections::HashMap;
use core::hash::Hash;
use core::fmt::Display;

pub struct Env<K, V> {
    top_level: HashMap<K, V>,
    scopes: Vec<HashMap<K, V>>,
}

impl<K, V> Env<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Env<K, V> {
        Env::with_toplevel(HashMap::new())
    }

    pub fn with_toplevel(top_level: HashMap<K, V>) -> Env<K, V> {
        Env {
            top_level,
            scopes: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.top_level.is_empty() && self.scopes.iter().all(|scope| scope.is_empty())
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.scopes.iter().any(|scope| scope.contains_key(key)) || self.top_level.contains_key(key)
    }

    fn current_scope(&mut self) -> &mut HashMap<K, V> {
        self.scopes.last_mut().unwrap_or(&mut self.top_level)
    }

    pub fn lookup(&self, key: K) -> Option<&V> {
        if !self.scopes.is_empty() {
            let mut depth = self.scopes.len();
            loop {
                depth -= 1;

                if depth == 0 || self.scopes[depth].contains_key(&key) {
                    break;
                }
            }
            self.scopes[depth]
                .get(&key)
                .or_else(|| self.top_level.get(&key))
        } else {
            self.top_level.get(&key)
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.current_scope().insert(key, value);
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
    }
}

impl<K: Display, V: Display> Display for Env<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_scope<K: Display, V: Display>(
            f: &mut std::fmt::Formatter<'_>,
            scope: &HashMap<K, V>,
        ) -> std::fmt::Result {
            let assocs: Vec<(&K, &V)> = scope.iter().collect();
            if assocs.is_empty() {
                write!(f, "∅")
            } else {
                write!(f, "{} : {}", assocs[0].0, assocs[0].1)?;
                for i in 1..assocs.len() {
                    write!(f, ", {} : {}", assocs[i].0, assocs[i].1)?;
                }
                Ok(())
            }
        }
        let mut first_non_empty_scope_index = 0;
        while first_non_empty_scope_index < self.scopes.len()
            && self.scopes[first_non_empty_scope_index].is_empty()
        {
            first_non_empty_scope_index += 1;
        }
        if first_non_empty_scope_index < self.scopes.len() {
            fmt_scope(f, &self.scopes[first_non_empty_scope_index])?;
            for i in (first_non_empty_scope_index + 1)..self.scopes.len() {
                let scope = &self.scopes[i];
                if !scope.is_empty() {
                    write!(f, "; ")?;
                    fmt_scope(f, &scope)?;
                }
            }
        }
        Ok(())
    }
}
