//
// Prototype demonstration of a mutable, indexable, value-type container.
//
// Some container elements (Characters in a String, key/value pairs in
// an open-addressing hash table) are not traversable with a fixed
// size offset, so incrementing/decrementing indices requires
// looking at the contents of the container.  The current interface for
// incrementing/decrementing indices of an Indexable is the usual ++i,
// --i. Therefore, for memory safety, a subset of indices need to keep a reference to
// the container's underlying data so that it can be inspected. But
// having multiple outstanding references to the underlying data makes
// internal mutation costly, as it is forced to copy (we can avoid this
// cost for some appends, but for mutations of the sort in-place
// algorithms do, there’s no tricky way out).
//
// The way out is to create multiple reference counts.  This code
// demonstrates using an open-addressing Dictionary as an example.
//
// Currently we use a data structure like this:
//
//    +---+
//    | * | Dictionary<K,V> (a struct)
//    +-|-+
//      |
//      V
//    +---+
//    | * | DictionaryBufferOwner<K,V>
//    +-|-+
//      |
//      V  FixedSizedRefArrayOfOptional<Pair<K,V>>
//    +-------------------------------------+
//    |                                     |
//    +-------------------------------------+
//
// In reality, we'd optimize by allocating the DictionaryBufferOwner
// /inside/ the FixedSizedRefArrayOfOptional, and override the dealloc
// method of DictionaryBufferOwner to do nothing but release its
// reference.
//
//    +---+
//    | * | Dictionary<K,V> (a struct)
//    +-|-+
//      |  +---+
//      |  |   |
//      |  |   V  FixedSizeArrayOfOptional<Pair<K,V>>
//  +---|--|------------------------------+
//  |   V  |                              |
//  | +----|-+                            |
//  | |    * | DictionaryBufferOwner<K,V> |
//  | +------+                            |
//  +-------------------------------------+
//  

import swift

class FixedSizedRefArrayOfOptional<T> : HeapBuffer<Int, T?>
{
  def initialize() -> FixedSizedRefArrayOfOptional<T> {
    for var i = 0; i < count; ++i {
      (elementStorage + i).initialize(.None)
    }
    return self
  }

  subscript(i: Int) -> T? {
  get:
    assert(i >= 0 && i < count)
    return (elementStorage + i).get()
  set:
    assert(i >= 0 && i < count)
    (elementStorage + i).set(value)
  }

  var count : Int {
    return value
  }

  destructor() {
    for var i = 0; i < count; ++i {
      (elementStorage + i).destroy()
    }
  }
}

struct DictionaryElement<KeyType: Hashable, ValueType> {
  var key: KeyType
  var value: ValueType
}

class DictionaryBufferOwner<KeyType: Hashable, ValueType> {
  typealias Element = DictionaryElement<KeyType, ValueType>
  typealias Buffer = FixedSizedRefArrayOfOptional<Element>

  init(capacity: Int = 2) {
    buffer = (Buffer.create(capacity, capacity) as Buffer)!.initialize()
  }
  var buffer: Buffer
}

struct DictionaryIndex<Element> : BidirectionalIndex {
  typealias Index = DictionaryIndex<Element>

  def __equal__(rhs: Index) -> Bool {
    return self.offset == rhs.offset
  }

  def pred() -> Index {
    var j = self.offset
    while --j > 0 {
      if buffer[j] {
        return Index(buffer, j)
      }
    }
    return self
  }

  def succ() -> Index {
    var i = self.offset + 1
    while i < buffer.count && !buffer[i] {
      ++i
    }
    return Index(buffer, i)
  }

  var buffer: FixedSizedRefArrayOfOptional<Element>
  var offset: Int
}

struct Dictionary<KeyType: Hashable, ValueType> : Indexable, Enumerable {
  typealias _Self = Dictionary<KeyType, ValueType>
  typealias BufferOwner = DictionaryBufferOwner<KeyType, ValueType>
  typealias Buffer = BufferOwner.Buffer
  typealias Element = BufferOwner.Element
  typealias Index = DictionaryIndex<Element>

  init(capacity: Int) {
    _owner = BufferOwner(capacity)
  }

  def startIndex() -> Index {
    return Index(_buffer, -1).succ()
  }

  def endIndex() -> Index {
    return Index(_buffer, _buffer.count).pred()
  }

  def __getitem__(i: Index) -> Element {
    return _buffer[i.offset]!
  }

  subscript(i: Index) -> Element {
    return __getitem__(i)
  set(keyValue):
    assert(keyValue.key == self[i].key)
    _buffer[i.offset] = .Some(keyValue)
  }

  var maxLoadFactor : Double {
    return 0.75 // must be < 1.0
  }

  subscript(key: KeyType) -> ValueType {
  get:
    return __getitem__(find(key).0).value
  set(value):
    _makeUniqueBuffer(Int(Double(_count + 1) / maxLoadFactor))
    var (i, found) = find(key)
    _buffer[i.offset] = Element(key, value)
    if !found {
      ++_count
    }
  }

  var capacity : Int {
    return _buffer.count
  }

  /// \brief Make sure this Dictionary holds a unique reference
  /// to its buffer having at least newCapacity elements
  def _makeUniqueBuffer(newCapacity: Int) {
    var isUnique = swift_isUniquelyReferenced(Builtin.castToObjectPointer(_owner))

    if capacity < newCapacity || !isUnique {
      if isUnique && newCapacity < capacity * 2 {
        newCapacity = capacity * 2
      }

      var newOwner = _Self(newCapacity)
      for i in 0.._buffer.count {
        if _buffer[i] {
          var x = _buffer[i]!
          newOwner[x.key] = x.value
        }
      }
      swift.swap(&self, &newOwner)
    }
  }

  def find(k : KeyType) -> (Index,Bool) {
    var h = k.hashValue()

    var start = Int(UInt64(h) % UInt64(capacity))
    var end = capacity

    for range in [start..end, 0..start] {
      for i in range {
        var filled = Bool(_buffer[i])
        if !filled || _buffer[i]!.key == k {
          return (Index(_buffer, i), filled)
        }
      }
    }
    return (Index(_buffer, end), false)
  }

  var count : Int {
    return _count
  }

  var _count: Int = 0
  var _owner: BufferOwner
  var _buffer: Buffer {
    return _owner.buffer
  }

  // Satisfying Enumerable
  def enumerate() -> IndexableGenerator<_Self, Range<Index>> {
    return IndexableGenerator(self, indices(self))
  }
}
