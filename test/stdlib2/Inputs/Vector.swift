struct VectorEnumeratorType<T> : Enumerator {
  typealias Element = T
  typealias _UnsafePtr = UnsafePointer<Element>

  var _base : _UnsafePtr
  var _size : Int

  func isEmpty() -> Bool {return _size == 0}
  func next() -> Element {
    debugTrap(_size > 0)
    var r = _base.get()
    ++_base
    --_size
    return r
  }
}

class Vector<T> : Enumerable {
  typealias Element = T

  typealias _UnsafePtr = UnsafePointer<Element>

  var _base : _UnsafePtr
  var _size : Int
  var _capacity : Int

  constructor() {
    _size = 0
    _capacity = 0
  }

  destructor {
    clear()
    _base.dealloc(_capacity)
  }

  func clear() {
    while _size > 0 {
      --_size
      (_base + _size).destroy()
    }
  }
  var length : Int {
    get { return _size }
  }

  var capacity : Int {
    get { return _capacity }
  }

  func reserve(cap : Int) {
    if cap > _capacity {
      var newCapacity = max(2*_capacity, cap)
      var newElements = _UnsafePtr.alloc(newCapacity)
      if _capacity > 0 {
        for i in 0.._size {
          (newElements + i).init((_base + i).move())
        }
        _base.dealloc(_capacity)
      }
      _base = newElements
      _capacity = newCapacity
    }
  }

  func append(item : Element) {
    if _size == _capacity {
      reserve(_size+1)
    }
    (_base + _size).init(item)
    ++_size
  }

  func insert(item : Element, i : Int) {
    if _size == _capacity {
      reserve(_size+1)
    }
    // create a hole.
    for var r = _size; r > i; --r {
      (_base + r).init((_base + r - 1).move())
    }
    (_base + i).init(item)
    ++_size
  }

  func popBack() {
    debugTrap(_size > 0)
    --_size
    (_base + _size).destroy()
  }

  func remove(i : Int) {
    (_base + i).destroy()
    for r in i+1.._size {
      (_base + r - 1).init((_base + r).move())
    }
    --_size
  }

  func each(f : (Element) -> Void) {
    for item in this {
      f(item)
    }
  }

  subscript (i : Int) -> Element {
    get {
      debugTrap(UInt(i) < UInt(_size))
      return (_base + i).get()
    }
    set {
      debugTrap(UInt(i) < UInt(_size))
      (_base + i).set(value)
    }
  }

  func copy() -> Vector<T> {
    var r = new Vector<T>
    r.reserve(length)
    for x in this {
      r.append(x)
    }
    return r
  }

  // Support enumeration protocol on vectors.

  typealias EnumeratorType = VectorEnumeratorType<Element>
  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(_base, _size)
  }
}

func [infix=160] == <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return lhs._base == rhs._base
}

func [infix=160] != <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return !(lhs == rhs)
}

func [infix=170] < <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return lhs._base < rhs._base
}

func [infix=170] > <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return rhs < lhs
}

func [infix=170] <= <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return !(rhs < lhs)
}

func [infix=170] >= <T>(lhs : Vector<T>, rhs : Vector<T>) -> Bool {
  return !(lhs < rhs)
}
