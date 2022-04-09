//! A hash map implemented with quadratic probing.

pub use std::collections::hash_map::{
    Drain, Entry, IntoIter, IntoKeys, IntoValues, Iter, IterMut, Keys, Values, ValuesMut,
};

use crate::AllocError;
use crate::FxBuildHasher;
use std::borrow::Borrow;
use std::collections::HashMap as StdHashMap;
use std::fmt;
use std::fmt::Debug;
use std::hash::{BuildHasher, Hash};
use std::ops::Index;

/// A hash map implemented with quadratic probing.
#[repr(transparent)]
pub struct HashMap<K, V, S = FxBuildHasher>(StdHashMap<K, V, S>);

impl<K, V> HashMap<K, V, FxBuildHasher> {
    /// Creates an empty `HashMap`.
    ///
    /// The hash map is initially created with a capacity of 0, so it will not allocate until it
    /// is first inserted into.
    #[must_use]
    #[inline]
    pub fn new() -> HashMap<K, V, FxBuildHasher> {
        HashMap::default()
    }
}

impl<K, V, S> HashMap<K, V, S> {
    #[inline]
    pub fn from_std(hash_map: StdHashMap<K, V, S>) -> Self {
        HashMap(hash_map)
    }

    #[inline]
    pub fn into_std(self) -> StdHashMap<K, V, S> {
        self.0
    }

    /// Creates an empty `HashMap` which will use the given hash builder to hash
    /// keys.
    ///
    /// The created map has the default initial capacity.
    ///
    /// Warning: `hash_builder` is normally randomly generated, and
    /// is designed to allow HashMaps to be resistant to attacks that
    /// cause many collisions and very poor performance. Setting it
    /// manually using this function can expose a DoS attack vector.
    ///
    /// The `hash_builder` passed should implement the [`BuildHasher`] trait for
    /// the HashMap to be useful, see its documentation for details.
    #[inline]
    pub fn with_hasher(hash_builder: S) -> HashMap<K, V, S> {
        HashMap(StdHashMap::with_hasher(hash_builder))
    }

    /// Returns the number of elements the map can hold without reallocating.
    ///
    /// This number is a lower bound; the `HashMap<K, V>` might be able to hold
    /// more, but is guaranteed to be able to hold at least this many.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// An iterator visiting all keys in arbitrary order.
    /// The iterator element type is `&'a K`.
    #[inline]
    pub fn keys(&self) -> Keys<'_, K, V> {
        self.0.keys()
    }

    /// Creates a consuming iterator visiting all the keys in arbitrary order.
    /// The map cannot be used after calling this.
    /// The iterator element type is `K`.
    #[inline]
    pub fn into_keys(self) -> IntoKeys<K, V> {
        self.0.into_keys()
    }

    /// An iterator visiting all values in arbitrary order.
    /// The iterator element type is `&'a V`.
    #[inline]
    pub fn values(&self) -> Values<'_, K, V> {
        self.0.values()
    }

    /// An iterator visiting all values mutably in arbitrary order.
    /// The iterator element type is `&'a mut V`.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        self.0.values_mut()
    }

    /// Creates a consuming iterator visiting all the values in arbitrary order.
    /// The map cannot be used after calling this.
    /// The iterator element type is `V`.
    #[inline]
    pub fn into_values(self) -> IntoValues<K, V> {
        self.0.into_values()
    }

    /// An iterator visiting all key-value pairs in arbitrary order.
    /// The iterator element type is `(&'a K, &'a V)`.
    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.0.iter()
    }

    /// An iterator visiting all key-value pairs in arbitrary order,
    /// with mutable references to the values.
    /// The iterator element type is `(&'a K, &'a mut V)`.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        self.0.iter_mut()
    }

    /// Returns the number of elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Clears the map, returning all key-value pairs as an iterator. Keeps the
    /// allocated memory for reuse.
    ///
    /// If the returned iterator is dropped before being fully consumed, it
    /// drops the remaining key-value pairs. The returned iterator keeps a
    /// mutable borrow on the vector to optimize its implementation.
    #[inline]
    pub fn drain(&mut self) -> Drain<'_, K, V> {
        self.0.drain()
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all pairs `(k, v)` for which `f(&k, &mut v)` returns `false`.
    /// The elements are visited in unsorted (and unspecified) order.
    #[inline]
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        self.0.retain(f);
    }

    /// Clears the map, removing all key-value pairs. Keeps the allocated memory
    /// for reuse.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns a reference to the map's [`BuildHasher`].
    #[inline]
    pub fn hasher(&self) -> &S {
        self.0.hasher()
    }
}

impl<K, V, S> HashMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher,
{
    /// Creates an empty `HashMap` with the specified capacity.
    ///
    /// The hash map will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the hash map will not allocate.
    #[inline]
    pub fn try_with_capacity(capacity: usize) -> Result<HashMap<K, V, S>, AllocError>
    where
        S: Default,
    {
        let mut m = StdHashMap::with_hasher(Default::default());
        m.try_reserve(capacity)?;
        Ok(HashMap(m))
    }

    /// Creates an empty `HashMap` with the specified capacity, using `hash_builder`
    /// to hash the keys.
    ///
    /// The hash map will be able to hold at least `capacity` elements without
    /// reallocating. If `capacity` is 0, the hash map will not allocate.
    ///
    /// Warning: `hash_builder` is normally randomly generated, and
    /// is designed to allow HashMaps to be resistant to attacks that
    /// cause many collisions and very poor performance. Setting it
    /// manually using this function can expose a DoS attack vector.
    ///
    /// The `hash_builder` passed should implement the [`BuildHasher`] trait for
    /// the HashMap to be useful, see its documentation for details.
    #[inline]
    pub fn try_with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Result<HashMap<K, V, S>, AllocError> {
        let mut m = StdHashMap::with_hasher(hash_builder);
        m.try_reserve(capacity)?;
        Ok(HashMap(m))
    }

    /// Tries to reserve capacity for at least `additional` more elements to be inserted
    /// in the given `HashMap<K, V>`. The collection may reserve more space to avoid
    /// frequent reallocations.
    ///
    /// # Errors
    ///
    /// If the capacity overflows, or the allocator reports a failure, then an error
    /// is returned.
    #[inline]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), AllocError> {
        self.0.try_reserve(additional)?;
        Ok(())
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    #[inline]
    pub fn try_entry(&mut self, key: K) -> Result<Entry<'_, K, V>, AllocError> {
        self.0.try_reserve(1)?;
        Ok(self.0.entry(key))
    }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(k)
    }

    /// Returns the key-value pair corresponding to the supplied key.
    ///
    /// The supplied key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn get_key_value<Q: ?Sized>(&self, k: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get_key_value(k)
    }

    /// Returns `true` if the map contains a value for the specified key.
    ///
    /// The key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.contains_key(k)
    }

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get_mut(k)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, [`None`] is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old
    /// value is returned. The key is not updated, though; this matters for
    /// types that can be `==` without being identical.
    #[inline]
    pub fn try_insert(&mut self, k: K, v: V) -> Result<Option<V>, AllocError> {
        self.0.try_reserve(1)?;
        Ok(self.0.insert(k, v))
    }

    /// Removes a key from the map, returning the value at the key if the key
    /// was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn remove<Q: ?Sized>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.remove(k)
    }

    /// Removes a key from the map, returning the stored key and value if the
    /// key was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but
    /// [`Hash`] and [`Eq`] on the borrowed form *must* match those for
    /// the key type.
    #[inline]
    pub fn remove_entry<Q: ?Sized>(&mut self, k: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.remove_entry(k)
    }
}

impl<K, V, S> Default for HashMap<K, V, S>
where
    S: Default,
{
    #[inline]
    fn default() -> Self {
        HashMap::with_hasher(Default::default())
    }
}

impl<K, V, S> PartialEq for HashMap<K, V, S>
where
    K: Eq + Hash,
    V: PartialEq,
    S: BuildHasher,
{
    #[inline]
    fn eq(&self, other: &HashMap<K, V, S>) -> bool {
        self.0.eq(&other.0)
    }
}

impl<K, V, S> Eq for HashMap<K, V, S>
where
    K: Eq + Hash,
    V: Eq,
    S: BuildHasher,
{
}

impl<K, V, S> Debug for HashMap<K, V, S>
where
    K: Debug,
    V: Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<K, Q: ?Sized, V, S> Index<&Q> for HashMap<K, V, S>
where
    K: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash,
    S: BuildHasher,
{
    type Output = V;

    /// Returns a reference to the value corresponding to the supplied key.
    ///
    /// # Panics
    ///
    /// Panics if the key is not present in the `HashMap`.
    #[inline]
    fn index(&self, key: &Q) -> &V {
        self.0.index(key)
    }
}

impl<'a, K, V, S> IntoIterator for &'a HashMap<K, V, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Iter<'a, K, V> {
        self.iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a mut HashMap<K, V, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> IterMut<'a, K, V> {
        self.iter_mut()
    }
}

impl<K, V, S> IntoIterator for HashMap<K, V, S> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    /// Creates a consuming iterator, that is, one that moves each key-value
    /// pair out of the map in arbitrary order. The map cannot be used after
    /// calling this.
    #[inline]
    fn into_iter(self) -> IntoIter<K, V> {
        self.0.into_iter()
    }
}

#[cfg(feature = "serde")]
mod serde {
    use crate::HashMap;
    use serde::de::{MapAccess, Visitor};
    use serde::{de::Error, Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt;
    use std::hash::{BuildHasher, Hash};
    use std::marker::PhantomData;

    impl<K, V, H> Serialize for HashMap<K, V, H>
    where
        K: Eq + Hash + Serialize,
        V: Serialize,
        H: BuildHasher,
    {
        #[inline]
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.collect_map(self)
        }
    }

    impl<'de, K, V, H> Deserialize<'de> for HashMap<K, V, H>
    where
        K: Eq + Hash + Deserialize<'de>,
        V: Deserialize<'de>,
        H: BuildHasher + Default + Deserialize<'de>,
    {
        #[inline]
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            struct MapVisitor<K, V, H> {
                _marker: PhantomData<HashMap<K, V, H>>,
            }

            impl<'de, K, V, H> Visitor<'de> for MapVisitor<K, V, H>
            where
                K: Eq + Hash + Deserialize<'de>,
                V: Deserialize<'de>,
                H: BuildHasher + Default + Deserialize<'de>,
            {
                type Value = HashMap<K, V, H>;

                #[inline]
                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a map")
                }

                #[inline]
                fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
                where
                    A: MapAccess<'de>,
                {
                    let cap = map.size_hint().unwrap_or(8).min(4096);
                    let mut values =
                        HashMap::try_with_capacity_and_hasher(cap, H::default()).map_err(A::Error::custom)?;

                    while let Some((key, value)) = map.next_entry()? {
                        values.try_insert(key, value).map_err(A::Error::custom)?;
                    }

                    Ok(values)
                }
            }

            let visitor = MapVisitor { _marker: PhantomData };
            deserializer.deserialize_map(visitor)
        }
    }
}
