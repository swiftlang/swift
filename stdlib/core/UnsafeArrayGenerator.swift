struct UnsafeArrayGenerator<T> : Generator, Enumerable {
  init(start: UnsafePointer<T>, length: Int) {
    _position = start
    _end = start + length
  }

  func next() -> T? {
    if _position == _end {
      return .None
    }
    return .Some((_position++).get())
  }

  func enumerate() -> UnsafeArrayGenerator {
    return self
  }
  
  var _position, _end: UnsafePointer<T>
}
