struct ArrayGeneratorType<T> : Generator {
  typealias Element = T
  var _base: UnsafePointer<T>
  var _size: Int

  func next() -> T? {
    if _size == 0 { return .None }
    debugTrap(_size > 0)
    var r = _base.get()
    ++_base
    --_size
    return r
  }
}

struct ArrayImp<T> {
  var _base: UnsafePointer<T>
  var _owner: Builtin.ObjectPointer

  var capacity: Int {
    get:
      var buf: ArrayBuffer<T> = Builtin.castFromObjectPointer(_owner)
      return buf.value._capacity
  }

  var count: Int {
     get:
       var buf: ArrayBuffer<T> = Builtin.castFromObjectPointer(_owner)
       return buf.value._count
     set(newValue):
       var buf: ArrayBuffer<T> = Builtin.castFromObjectPointer(_owner)
       buf.value._count = newValue
  }

  init() {
    var buffer = ArrayBuffer<T>.create((0, 0), 0)
    _base = buffer.elementStorage
    _owner = Builtin.castToObjectPointer(buffer)
  }

  init(size: Int, t: T) {
    debugTrap(size >= 0)
    var buffer = ArrayBuffer<T>.create((0, size), size)
    _base = buffer.elementStorage
    _owner = Builtin.castToObjectPointer(buffer)
    for count = 0; count < size; ++count {
      (_base + count).initialize(t)
    }
  }

  static func _createFrom(base: UnsafePointer<T>,
                          owner: Builtin.ObjectPointer) -> ArrayImp {
    var v = ArrayImp()
    v._base = base
    v._owner = owner
    return v
  }

   func copy(newCap: Int) -> ArrayImp<T> {
        if (newCap < capacity) {
            newCap = capacity
        }
        var buffer = ArrayBuffer<T>.create((0, newCap), newCap)
        var base = buffer.elementStorage
        var owner = Builtin.castToObjectPointer(buffer)
        var v = ArrayImp._createFrom(base, owner)
        for v.count = 0; v.count < count; ++v.count {
            (v._base + v.count).initialize(self[v.count])
        }
        return v
   }

  func clear () {
      while count > 0 {
        --count
        (_base + count).destroy()
      }
  }

  func reserve(cap: Int) {
    if cap > capacity {
      var newCapacity = max(2*capacity, cap)
      var newBuffer = ArrayBuffer<T>.create((0, newCapacity), newCapacity)
      var newBase = newBuffer.elementStorage
      var newOwner = Builtin.castToObjectPointer(newBuffer)
      if capacity > 0 {
        for newBuffer.value._count = 0; newBuffer.value._count < count;
                                                     ++newBuffer.value._count {
          (newBase + newBuffer.value._count).initialize(
                                       (_base + newBuffer.value._count).move())
        }
        count = 0
      }
      _base = newBase
      _owner = newOwner
    }
  }

  func append(item: T) {
    if count == capacity {
      reserve(count+1)
    }
    (_base + count).initialize(item)
    ++count
  }

  func insert(item: T, i: Int) {
    if count == capacity {
      reserve(count+1)
    }
    // create a hole.
    for var r = count; r > i; --r {
      (_base + r).initialize((_base + r - 1).move())
    }
    (_base + i).initialize(item)
    ++count
  }

  func popLast() {
    debugTrap(count > 0)
    --count
    (_base + count).destroy()
  }

  func remove(i: Int) {
    debugTrap(count > i)
    (_base + i).destroy()
    for r in i+1..count {
      (_base + r - 1).initialize((_base + r).move())
    }
    --count
  }

  subscript (i: Int) -> T {
  get:
    debugTrap(i < count)
    return (_base + i).get()
  set:
    debugTrap(i < count)
    (_base + i).set(value)
  }
}

struct Array<T> : Enumerable {
   var _value: ArrayImp<T>

  init() {
     _value = ArrayImp<T>()
  }

  init(size: Int, t: T) {
     _value = ArrayImp<T>(size, t)
  }

  func clear() {
    _makeUnique(0)
    _value.clear()
  }

  var count: Int {
    get: return _value.count
  }

  var isEmpty: Bool {
    get: return _value.count == 0
  }

  var capacity: Int {
    get: return _value.capacity
  }

  func _makeUnique(newCap: Int) {
    if !swift_isUniquelyReferenced(_value._owner) {
        _value = _value.copy(newCap)
    }
  }

  func reserve(cap: Int) {
    _makeUnique(cap)
    _value.reserve(cap)
  }

  func append(item: T) {
    _makeUnique(count+1)
    _value.append(item)
  }

  func insert(item: T, i: Int) {
    _makeUnique(count+1)
    _value.insert(item, i)
  }

  func popLast() {
    _makeUnique(count-1)
    _value.popLast()
  }

  func remove(i: Int) {
    _makeUnique(count-1)
    _value.remove(i)
  }

  subscript (i: Int) -> T {
  get:
    return _value[i]
  set:
    debugTrap(i < count)
    _makeUnique(capacity)
    (_value._base + i).set(value)
  }

  /// \brief Move the elements into a slice and return it, leaving
  /// the Array empty.
  func takeArray() -> T[] {
    _makeUnique(capacity)
    // Move elements into a new owner object
    var owner = Array()
    self.swap(&owner)

    typealias TakenArray = T[]
    return TakenArray.convertFromHeapArray(
      base: owner._value._base.value, 
      length: owner._value.count.value,
      owner: owner._value._owner)
  }

  func swap(other: @inout Array) {
    swift.swap(&self._value, &other._value)
  }

  // Support enumeration protocol on vectors.
  // FIXME: <rdar://problem/12202655> prevents us from removing GeneratorType.
  // We need to be checking the explicit conformances of Array before
  // checking the type Array<Int>.GeneratorType in 
  // test/stdlib/Algorithm.swift.
  typealias GeneratorType = ArrayGeneratorType<T>
  func enumerate() -> ArrayGeneratorType<T> {
    return ArrayGeneratorType(_value._base, count)
  }
}
