struct VectorRange<T> : Range {
  typealias Element = T
  typealias _UnsafePtr = UnsafePointer<Element>

  var _base : _UnsafePtr
  var _size : Int

  func isEmpty() -> Bool {return _size == 0}
  func getFirstAndAdvance() -> Element {
    assert(_size > 0)
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
    if (cap > _capacity) {
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
    if (_size == _capacity) {
      reserve(_size+1)
    }
    (_base + _size).init(item)
    ++_size
  }

  func popBack() {
    assert(_size > 0)
    --_size
    (_base + _size).destroy()
  }

  func each(f : (Element) -> Void) {
    for item in this {
      f(item)
    }
  }

  subscript (i : Int) -> Element {
    get {
      assert(i < _size)
      return (_base + i).get()
    }
    set {
      assert(i < _size)
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

  typealias Elements = VectorRange<Element>
  func getElements() -> Elements {
    return Elements(_base, _size)
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
