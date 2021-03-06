use crate::core::{Scheme, Type};
use std::collections::HashMap;

lazy_static! {
    pub static ref PRIMITIVES: HashMap<&'static str, Scheme<'static>> = {
        let mut prims = HashMap::new();
        prims.insert(
            "error",
            Scheme {
                variables: vec!["A".to_owned()],
                type_: Type::QVar("A".to_owned()),
            },
        );
        prims
    };
}
