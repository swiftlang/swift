# Supporting Types

Use wrappers, indices, and iterators in operations like slicing, flattening, and
reversing a collection.

## Topics

### Slices

- ``Swift/Slice``

### Range Expressions

- ``Swift/PartialRangeUpTo``
- ``Swift/PartialRangeThrough``
- ``Swift/PartialRangeFrom``
- ``Swift/RangeExpression``
- ``Swift/UnboundedRange_``

### Type-Erasing Wrappers

- ``Swift/AnySequence``
- ``Swift/AnyCollection``
- ``Swift/AnyBidirectionalCollection``
- ``Swift/AnyRandomAccessCollection``
- ``Swift/AnyIterator``
- ``Swift/AnyIndex``
- ``Swift/AnyHashable``

### Lazy Wrappers

Use these lazy wrappers to defer any filtering or transformation of collection elements
until elements are accessed.

- ``Swift/LazySequence``
- ``Swift/LazyMapSequence``
- ``Swift/LazyFilterSequence``
- ``Swift/LazyPrefixWhileSequence``
- ``Swift/LazyDropWhileSequence``
- ``Swift/LazyCollection``
- ``Swift/LazyDropWhileCollection``
- ``Swift/LazyFilterCollection``
- ``Swift/LazyMapCollection``
- ``Swift/LazyPrefixWhileCollection``

### Wrappers for Algorithms

Many collection operations are performed by wrapping a collection in another type,
instead of copying the collection's contents.

- ``Swift/CollectionDifference``
- ``Swift/DropFirstSequence``
- ``Swift/DropWhileSequence``
- ``Swift/EnumeratedSequence``
- ``Swift/FlattenCollection``
- ``Swift/FlattenSequence``
- ``Swift/JoinedSequence``
- ``Swift/PrefixSequence``
- ``Swift/Repeated``
- ``Swift/ReversedCollection``
- ``Swift/StrideTo``
- ``Swift/StrideThrough``
- ``Swift/UnfoldSequence``
- ``Swift/Zip2Sequence``

### Collections of Indices

- ``Swift/DefaultIndices``

### Indices and Iterators

Index and iterator types for other sequence and collection types in the standard
library.

- ``Swift/IteratorSequence``
- ``Swift/IndexingIterator``
- ``Swift/EnumeratedIterator``
- ``Swift/SetIterator``
- ``Swift/StrideThroughIterator``
- ``Swift/StrideToIterator``

### Deprecated

- ``Swift/DictionaryIndex``
- ``Swift/SetIndex``
- ``Swift/CountableClosedRange``
- ``Swift/CountablePartialRangeFrom``
- ``Swift/CountableRange``
