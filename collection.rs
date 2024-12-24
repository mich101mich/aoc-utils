use super::*;

pub trait CollectionExt<T> {
    fn filtered(&self, filter: impl FnMut(&T) -> bool) -> Self
    where
        T: Clone;
}
pub trait MapCollectionExt<K, V> {
    fn filtered(&self, filter: impl FnMut(&K, &mut V) -> bool) -> Self
    where
        K: Clone,
        V: Clone;
}

macro_rules! filtered_impl {
    ($ty: ty) => {
        fn filtered(&self, filter: impl FnMut(&$ty) -> bool) -> Self
        where
            $ty: Clone,
        {
            let mut ret = self.clone();
            ret.retain(filter);
            ret
        }
    };
    ($k: ty, $v: ty) => {
        fn filtered(&self, filter: impl FnMut(&$k, &mut $v) -> bool) -> Self
        where
            $k: Clone,
            $v: Clone,
        {
            let mut ret = self.clone();
            ret.retain(filter);
            ret
        }
    };
}

impl<T> CollectionExt<T> for Vec<T> {
    filtered_impl!(T);
}

impl<T> CollectionExt<T> for VecDeque<T> {
    filtered_impl!(T);
}

impl<T> CollectionExt<T> for HashSet<T> {
    filtered_impl!(T);
}

impl<T: Ord> CollectionExt<T> for BTreeSet<T> {
    filtered_impl!(T);
}

impl<K: Hash, V> MapCollectionExt<K, V> for HashMap<K, V> {
    filtered_impl!(K, V);
}

impl<K: Ord, V> MapCollectionExt<K, V> for BTreeMap<K, V> {
    filtered_impl!(K, V);
}
