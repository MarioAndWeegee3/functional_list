use std::rc::Rc;
use std::fmt;
use std::iter::FromIterator;
use std::borrow::Borrow;

struct Node<T> {
    value: T,
    next: Option<Rc<Node<T>>>,
}

impl <T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.next == other.next
    }
}

/// A singly linked list. Implemented using functional
/// algorithms, with no mutation.
pub struct List<T> {
    head: Option<Rc<Node<T>>>,
}

impl <T: Eq + PartialEq> Eq for List<T> {}

impl <T> Clone for List<T> {
    fn clone(&self) -> Self {
        List {
            head: self.head.clone()
        }
    }
}

impl <T: fmt::Debug> fmt::Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ ")?;
        self.for_each(
            |v| write!(f, "{:?} ", v).unwrap()
        );
        write!(f, "]")
    }
}

impl <T: PartialEq> PartialEq for List<T> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head
    }
}

impl <T> Default for List<T> {
    fn default() -> Self {
        Self::nil()
    }
}

impl <T> List<T> {
    /// Returns a list consisting of a single element
    pub fn single(v: T) -> Self {
        Self::cons(v, Self::nil())
    }

    /// Returns an empty list
    pub const fn nil() -> Self {
        Self {
            head: None,
        }
    }

    /// Returns the first element in the list,
    /// or if it is empty, returns None
    pub fn head(&self) -> Option<&T> {
        match &self.head {
            Some(v) => Some(&v.value),
            None => None
        }
    }

    /// Returns every element in the list except for the first.
    /// If the list has <= 1 elements, returns None.
    pub fn tail(&self) -> Option<List<T>> {
        match &self.head {
            Some(v) => Some(List { head: v.next.clone() }),
            None => None
        }
    }

    /// Returns every element in the list except for the first,
    /// as a list of references.
    pub fn tail_ref(&self) -> Option<List<&T>> {
        self.ref_list().tail()
    }

    /// Returns true if the list is empty
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Returns the number of elements in the list
    pub fn len(&self) -> usize {
        let mut len = 0;
        self.for_each(|_| len += 1);
        len
    }

    /// Returns the first `n` elements of the list.
    pub fn take(&self, n: usize) -> List<&T> {
        self.iter().take(n).collect()
    }

    /// Returns the first continuous list of elements that match the condition.
    pub fn take_while(&self, mut f: impl FnMut(&T) -> bool) -> List<&T> {
        self.iter().take_while(|x| f(*x)).collect()
    }

    /// Returns a new list with `head` as its first element,
    /// and `tail` as the rest of the list.
    pub fn cons(head: T, tail: Self) -> Self {
        let head = Node {
            value: head,
            next: tail.head,
        };
        List {
            head: Some(Rc::new(head)),
        }
    }

    /// Equivalent to `List::cons(value, (*self).clone())`
    pub fn prepend(&self, value: T) -> Self {
        Self::cons(value, (*self).clone())
    }

    /// Iterate through the elements of the list
    pub fn for_each<'s>(&'s self, mut f: impl FnMut(&'s T)) {
        for v in self.iter() {
            f(v)
        }
    }

    /// Returns an iterator through the elements of the list.
    pub fn iter(&self) -> ListIter<T> {
        ListIter {
            node: &self.head,
        }
    }

    /// Returns a list of references to this list's elements.
    pub fn ref_list(&self) -> List<&T> {
        let mut list = List::nil();

        self.for_each(|x| list = list.prepend(x));

        list.reversed()
    }

    pub fn fold<'s, R>(&'s self, initial: R, f: impl FnMut(R, &'s T) -> R) -> R {
        self.iter().fold(initial, f)
    }

    pub fn reduce<'s>(&'s self, f: impl FnMut(Option<&'s T>, &'s T) -> Option<&'s T>) -> Option<&'s T>{
        let initial = self.head.as_ref();
        let initial = initial.map(|v| &v.value);
        self.fold(initial, f)
    }

    /// Returns a list of references to this list's elements
    /// in reverse order
    pub fn rev(&self) -> List<&T> {
        self.fold(
            List::nil(),
            |acc, x|
                List::cons(x, acc)
        )
    }

    /// Append the `other` list to the end of this list,
    /// returning a list of references to both lists'
    /// elements.
    pub fn append<'a>(&'a self, other: &'a Self) -> List<&'a T> {
        let mut list = List::nil();
        self.for_each(|x| list = list.prepend(x));
        other.for_each(|x| list = list.prepend(x));
        list.reversed()
    }

    /// Build a new list by applying the passed function to
    /// each of the list's elements.
    pub fn map<R>(&self, f: impl Fn(&T) -> R) -> List<R> {
        match &self.head {
            None => List::nil(),
            Some(head) => {
                let tail = self.tail().unwrap();
                let head = f(&head.value);
                let tail = tail.map(f);
                List::cons(head, tail)
            }
        }
    }

    /// Zip two lists together, forming a list of key-value pairs.
    pub fn zip<'a, U>(&'a self, other: &'a List<U>) -> List<(&'a T, &'a U)> {
        let (a, b) = (self.iter(), other.iter());

        a.zip(b).collect()
    }
}

impl <T: Clone> List<T> {
    /// Returns a reversed clone of this list.
    /// Equivalent to `self.rev().cloned()`.
    pub fn reversed(&self) -> Self {
        self.rev().cloned()
    }
}

impl <'v, T: Clone> List<&'v T> {
    /// Clones the elements of the list from the references.
    pub fn cloned(self) -> List<T> {
        let mut list = List::nil();
        self.for_each(|v| list = list.prepend((**v).clone()));
        list.fold(
            List::nil(),
            |acc, x|
                List::cons(x.clone(), acc)
        )
    }
}

impl <T> List<List<T>> {
    /// Concatenate the inner lists to form a single list.
    pub fn join(&self) -> List<&T> {
        self.fold(
            List::nil(),
            |acc, x|
                acc.append(&x.ref_list()).cloned()
        )
    }
}

/// A `Map` from `K` to `V`.
pub type ListMap<K, V> = List<(K, V)>;

impl <K, V> ListMap<K, V>
    where K: Eq
{
    /// Get the element that matches the key,
    /// if it exists. Searches the elements in order.
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Eq,
    {
        for (k, v) in self.iter() {
            if k.borrow() == key {
                return Some(v)
            }
        }
        None
    }

    /// Equivalent to `self.prepend((key, value))`
    pub fn insert(&self, key: K, value: V) -> Self {
        self.prepend((key, value))
    }
}

/// An iterator by reference over the values in a list.
pub struct ListIter<'list, T> {
    node: &'list Option<Rc<Node<T>>>,
}

impl <'list, T> Iterator for ListIter<'list, T> {
    type Item = &'list T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.node {
            None => None,
            Some(head) => {
                self.node = &head.next;
                Some(&head.value)
            }
        }
    }
}

impl <T: Clone> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        let mut list = List::nil();
        while let Some(v) = iter.next() {
            list = list.prepend(v);
        }
        list.reversed()
    }
}