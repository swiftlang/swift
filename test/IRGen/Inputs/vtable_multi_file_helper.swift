class EmptyBase {}
class Subclass : EmptyBase {
  class var classProp: String { return "abc" }
}

class SillySequence : Sequence, IteratorProtocol {
  typealias Element = Int

  var storedProperty: Int = 0

  func makeIterator() -> SillySequence {
    return self
  }

  func next() -> Int? {
    return nil
  }
}

class Holder {
  func getSillySequence() -> SillySequence {
    return SillySequence()
  }
}

class Base {
  func method() {}
}