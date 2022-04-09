//! A library for fallible hash collections.

pub mod hash_map;
pub mod hash_set;

pub use fallacy_alloc::AllocError;
pub use fxhash::{FxBuildHasher, FxHasher};

#[doc(inline)]
pub use hash_map::HashMap;

#[doc(inline)]
pub use hash_set::HashSet;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
