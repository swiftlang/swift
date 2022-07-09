public protocol IteratorProtocol {
  associatedtype Element

  func next() -> Element?
}

public func doSomething() -> some IteratorProtocol {
  return doSomethingInternal()
}

func doSomethingInternal() -> some IteratorProtocol {
  return SomeIterator()
}

struct SomeIterator: IteratorProtocol {
  public typealias Element = Int

  public func next() -> Int? {
    return nil
  }
}
