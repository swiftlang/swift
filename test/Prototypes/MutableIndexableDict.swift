// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

// General Mutable, CollectionType, Value-Type Collections
// =================================================
//
// Basic copy-on-write (COW) requires a container's data to be copied
// into new storage before it is modified, to avoid changing the data
// of other containers that may share the data.  There is one
// exception: when we know the container has the only reference to the
// data, we can elide the copy.  This COW optimization is crucial for
// the performance of mutating algorithms.
//
// Some container elements (Characters in a String, key/value pairs in
// an open-addressing hash table) are not traversable with a fixed
// size offset, so incrementing/decrementing indices requires looking
// at the contents of the container.  The current interface for
// incrementing/decrementing indices of an CollectionType is the usual ++i,
// --i. Therefore, for memory safety, the indices need to keep a
// reference to the container's underlying data so that it can be
// inspected.  But having multiple outstanding references to the
// underlying data defeats the COW optimization.
//
// The way out is to count containers referencing the data separately
// from indices that reference the data.  When deciding to elide the
// copy and modify the data directly---as long as we don't violate
// memory safety of any outstanding indices---we only need to be
// sure that no other containers are referencing the data.
//
// Implementation
// --------------
//
// Currently we use a data structure like this:
//
//    Dictionary<K,V> (a struct)
//    +---+
//    | * | 
//    +-|-+
//      |
//      V  DictionaryBufferOwner<K,V>
//    +----------------+
//    | * [refcount#1] |
//    +-|--------------+
//      |
//      V  FixedSizedRefArrayOfOptional<Pair<K,V>>
//    +-----------------------------------------+
//    | [refcount#2]    [...element storage...] |
//    +-----------------------------------------+
//      ^
//    +-|-+
//    | * | Dictionary<K,V>.Index (a struct)
//    +---+
//
//
// In reality, we'd optimize by allocating the DictionaryBufferOwner
// /inside/ the FixedSizedRefArrayOfOptional, and override the dealloc
// method of DictionaryBufferOwner to do nothing but release its
// reference.
//
//    Dictionary<K,V> (a struct)
//    +---+
//    | * | 
//    +-|-+
//      |  +---+
//      |  |   |
//      |  |   V  FixedSizeRefArrayOfOptional<Pair<K,V>>
//  +---|--|-------------------------------------------+
//  |   |  |                                           |
//  |   |  |  [refcount#2]   [...element storage...]   |
//  |   |  |                                           |
//  |   V  | DictionaryBufferOwner<K,V>                |
//  | +----|--------------+                            |
//  | |    * [refcount#1] |                            |
//  | +-------------------+                            |
//  +--------------------------------------------------+
//      ^
//    +-|-+
//    | * | Dictionary<K,V>.Index (a struct)
//    +---+
//
// Index Invalidation
// ------------------
//
// Indexing a container, "c[i]", uses the integral offset stored in
// the index to access the elements referenced by the container.  The
// buffer referenced by the index is only used to increment and
// decrement the index.  Most of the time, these two buffers will be
// identical, but they need not always be.  For example, if one
// ensures that a Dictionary has sufficient capacity to avoid
// reallocation on the next element insertion, the following works
//
//    var (i, found) = d.find(k) // i is associated with d's buffer
//    if found {
//       var e = d            // now d is sharing its data with e
//       e[newKey] = newValue // e now has a unique copy of the data
//       return e[i]          // use i to access e
//    }
//
// The result should be a set of iterator invalidation rules familiar
// to anyone familiar with the C++ standard library.  Note that
// because all accesses to a dictionary buffer are bounds-checked,
// this scheme never compromises memory safety.
import Swift

class FixedSizedRefArrayOfOptionalStorage<T> : _HeapBufferStorage<Int, T?> {
  deinit {
    let buffer = Buffer(self)
    for i in 0..<buffer.value {
      (buffer.baseAddress + i).destroy()
    }
  }
}

struct FixedSizedRefArrayOfOptional<T>
{
  typealias Storage = FixedSizedRefArrayOfOptionalStorage<T>
  let buffer: Storage.Buffer
  
  init(capacity: Int)
  {
    buffer = Storage.Buffer(Storage.self, capacity, capacity)
    for var i = 0; i < capacity; ++i {
      (buffer.baseAddress + i).initialize(.None)
    }

    buffer.value = capacity
  }

  subscript(i: Int) -> T? {
    get {
      assert(i >= 0 && i < buffer.value)
      return (buffer.baseAddress + i).memory
    }
    nonmutating
    set {
      assert(i >= 0 && i < buffer.value)
      (buffer.baseAddress + i).memory = newValue
    }
  }

  var count: Int {
    get {
      return buffer.value
    }
    nonmutating
    set(newValue) {
      buffer.value = newValue
    }
  }
}

struct DictionaryElement<Key: Hashable, Value> {
  var key: Key
  var value: Value
}

class DictionaryBufferOwner<Key: Hashable, Value> {
  typealias Element = DictionaryElement<Key, Value>
  typealias Buffer = FixedSizedRefArrayOfOptional<Element>

  init(minimumCapacity: Int = 2) {
    // Make sure there's a representable power of 2 >= minimumCapacity
    assert(minimumCapacity <= (Int.max >> 1) + 1)

    var capacity = 2
    while capacity < minimumCapacity {
      capacity <<= 1
    }
    buffer = Buffer(capacity: capacity)
  }

  var buffer: Buffer
}

func == <Element>(lhs: DictionaryIndex<Element>, rhs: DictionaryIndex<Element>) -> Bool {
  return lhs.offset == rhs.offset
}

struct DictionaryIndex<Element> : BidirectionalIndexType {
  typealias Index = DictionaryIndex<Element>

  func predecessor() -> Index {
    var j = self.offset
    while --j > 0 {
      if buffer[j] != nil {
        return Index(buffer: buffer, offset: j)
      }
    }
    return self
  }

  func successor() -> Index {
    var i = self.offset + 1
    // FIXME: Can't write the simple code pending
    // <rdar://problem/15484639> Refcounting bug
    while i < buffer.count /*&& !buffer[i]*/ {
      // FIXME: workaround for <rdar://problem/15484639>
      if buffer[i] != nil {
        break
      }
      // end workaround
      ++i
    }
    return Index(buffer: buffer, offset: i)
  }

  var buffer: FixedSizedRefArrayOfOptional<Element>
  var offset: Int
}

struct Dictionary<Key: Hashable, Value> : CollectionType, SequenceType {
  typealias _Self = Dictionary<Key, Value>
  typealias BufferOwner = DictionaryBufferOwner<Key, Value>
  typealias Buffer = BufferOwner.Buffer
  typealias Element = BufferOwner.Element
  typealias Index = DictionaryIndex<Element>

  /// \brief Create a dictionary with at least the given number of
  /// elements worth of storage. The actual capacity will be the
  /// smallest power of 2 that's >= minimumCapacity.
  init(minimumCapacity: Int = 2) {
    _owner = BufferOwner(minimumCapacity: minimumCapacity)
  }

  var startIndex: Index {
    return Index(buffer: _buffer, offset: -1).successor()
  }

  var endIndex: Index {
    return Index(buffer: _buffer, offset: _buffer.count)
  }

  subscript(i: Index) -> Element {
    get {
      return  _buffer[i.offset]!
    }
    set(keyValue) {
      assert(keyValue.key == self[i].key)
      _buffer[i.offset] = .Some(keyValue)
    }
  }

  var _maxLoadFactorInverse = 1.0 / 0.75

  var maxLoadFactor : Double {
    get {
      return 1.0 / _maxLoadFactorInverse
    }
    mutating
    set(newValue) {
      // 1.0 might be useful for testing purposes; anything more is
      // crazy
      assert(newValue <= 1.0)
      _maxLoadFactorInverse = 1.0 / newValue
    }
  }

  subscript(key: Key) -> Value {
    get {
      return self[find(key).0].value
    }
    mutating
    set(value) {
      var (i, found) = find(key)

      // count + 2 below ensures that we don't fill in the last hole
      var minCapacity = found
        ? capacity
        : max(Int(Double(count + 1) * _maxLoadFactorInverse), count + 2)

      if (_ensureUniqueBuffer(minCapacity)) {
        i = find(key).0
      }
      _buffer[i.offset] = Element(key: key, value: value)

      if !found {
        ++_count
      }
    }
  }

  var capacity : Int {
    return _buffer.count
  }

  var _bucketMask : Int {
    return capacity - 1
  }

  /// \brief Ensure this Dictionary holds a unique reference to its
  /// buffer having at least minimumCapacity elements.  Return true
  /// iff this results in a change of capacity.
  mutating func _ensureUniqueBuffer(minimumCapacity: Int) -> Bool {
    var isUnique: Bool = isUniquelyReferencedNonObjC(&_owner)

    if !isUnique || capacity < minimumCapacity {
      var newOwner = _Self(minimumCapacity: minimumCapacity)
      print("reallocating with isUnique: \(isUnique) and capacity \(capacity)=>\(newOwner.capacity)")

      for i in 0..<capacity {
        var x = _buffer[i]
        if x != nil {
          if capacity == newOwner.capacity {
            newOwner._buffer[i] = x
          }
          else {
            newOwner[x!.key] = x!.value
          }
        }
      }

      newOwner._count = count
      Swift.swap(&self, &newOwner)
      return self.capacity != newOwner.capacity
    }
    return false
  }

  func _bucket(k: Key) -> Int {
    return k.hashValue & _bucketMask
  }

  func _next(bucket: Int) -> Int {
    return (bucket + 1) & _bucketMask
  }

  func _prev(bucket: Int) -> Int {
    return (bucket - 1) & _bucketMask
  }

  func _find(k: Key, startBucket: Int) -> (Index,Bool) {
    var bucket = startBucket
    

    // The invariant guarantees there's always a hole, so we just loop
    // until we find one.
    assert(count < capacity)
    while true {
      var keyVal = _buffer[bucket]
      if (keyVal == nil) || keyVal!.key == k {
        return (Index(buffer: _buffer, offset: bucket), keyVal != nil)
      }
      bucket = _next(bucket)
    }
  }

  func find(k: Key) -> (Index,Bool) {
    return _find(k, startBucket: _bucket(k))
  }

  mutating
  func deleteKey(k: Key) -> Bool {
    var start = _bucket(k)
    var (pos, found) = _find(k, startBucket: start)

    if !found {
      return false
    }

    // remove the element
    _buffer[pos.offset] = .None
    --_count

    // If we've put a hole in a chain of contiguous elements, some
    // element after the hole may belong where the new hole is.
    var hole = pos.offset

    // Find the last bucket in the contiguous chain
    var lastInChain = hole
    for var b = _next(lastInChain); _buffer[b] != nil; b = _next(b) {
      lastInChain = b
    }

    // Relocate out-of-place elements in the chain, repeating until
    // none are found.
    while hole != lastInChain {
      // Walk backwards from the end of the chain looking for
      // something out-of-place.
      var b: Int
      for b = lastInChain; b != hole; b = _prev(b) {

        var idealBucket = _bucket(_buffer[b]!.key)

        // Does this element belong between start and hole?  We need
        // two separate tests depending on whether [start,hole] wraps
        // around the end of the buffer
        var c0 = idealBucket >= start
        var c1 = idealBucket <= hole
        if start < hole ? (c0 && c1) : (c0 || c1) {
          break // found it
        }
      }

      if b == hole { // No out-of-place elements found; we're done adjusting
        break
      }

      // Move the found element into the hole
      _buffer[hole] = _buffer[b]
      _buffer[b] = .None
      hole = b
    }

    return true
  }

  var count : Int {
    return _count
  }

  var _count: Int = 0
  var _owner: BufferOwner
  var _buffer: Buffer {
    return _owner.buffer
  }

  // Satisfying SequenceType
  func generate() -> IndexingGenerator<_Self> {
    return IndexingGenerator(self)
  }
}

func == <K: Equatable, V: Equatable>(
  lhs: Dictionary<K,V>, rhs: Dictionary<K,V>
) -> Bool {
  if lhs.count != rhs.count {
    return false
  }

  for lhsElement in lhs {
    var (pos, found) = rhs.find(lhsElement.key)
    // FIXME: Can't write the simple code pending
    // <rdar://problem/15484639> Refcounting bug
    /*
    if !found || rhs[pos].value != lhsElement.value {
      return false
    }
    */
    if !found {
      return false
    }
    if rhs[pos].value != lhsElement.value {
      return false
    }
  }
  return true
}

func != <K: Equatable, V: Equatable>(
  lhs: Dictionary<K,V>, rhs: Dictionary<K,V>
) -> Bool {
  return !(lhs == rhs)
}

//
// Testing
//

// CHECK: testing
print("testing")

var d0 = Dictionary<Int, String>()
d0[0] = "zero"
print("Inserting #2")
// CHECK-NEXT: Inserting #2
d0[1] = "one"
// CHECK-NEXT: reallocating with isUnique: true and capacity 2=>4

d0[2] = "two"
var d1 = d0
print("copies are equal: \(d1 == d0)")
// CHECK-NEXT: copies are equal: true

d1[3] = "three"
// CHECK-NEXT: reallocating with isUnique: false and capacity 4=>8
print("adding a key to one makes them unequal: \(d1 != d0)")
// CHECK-NEXT: adding a key to one makes them unequal: true

d1.deleteKey(3)
print("deleting that key makes them equal again: \(d1 == d0)")

// ---------

class X : CustomStringConvertible {
   var constructed : Bool
   var id = 0

   init() {print("X()"); constructed = true}
   init(_ anID : Int) {
      print("X(\(anID))")
     id = anID; constructed = true
   }

   deinit {
      print("~X(\(id))")
      constructed = false
   }

   var description: String {
     return "X(\(id))"
   }
}

extension String {
  init(_ x: X) {
    self = "X(\(x.id))"
  }
}

func display(v : Dictionary<Int, X>) {
  print("[ ", appendNewline: false)
  var separator = ""
  for p in v {
    print("\(separator) \(p.key) : \(p.value)", appendNewline: false)
    separator = ", "
  }
  print(" ]")
}

func test() {
  var v = Dictionary<Int, X>()
  v[1] = X(1)
  // CHECK: X(1)
  display(v)
  // CHECK-NEXT: [  1 : X(1) ]
  v[2] = X(2)
  // CHECK-NEXT: X(2)
  // CHECK-NEXT: reallocating with isUnique: true and capacity 2=>4
  display(v)
  // CHECK-NEXT: [  1 : X(1),  2 : X(2) ]
  v[3] = X(3)
  // CHECK-NEXT: X(3)
  display(v)
  // CHECK-NEXT: [  1 : X(1),  2 : X(2),  3 : X(3) ]
  v[4] = X(4)
  // CHECK-NEXT: X(4)
  // CHECK-NEXT: reallocating with isUnique: true and capacity 4=>8
  display(v)
  // CHECK-NEXT: [  1 : X(1),  2 : X(2),  3 : X(3),  4 : X(4) ]
  // CHECK: ~X(1)
  // CHECK: ~X(2)
  // CHECK: ~X(3)
  // CHECK: ~X(4)
}

test()
