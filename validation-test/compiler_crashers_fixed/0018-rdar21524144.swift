// RUN: not %target-swift-frontend %s -typecheck

public protocol Indexable {
  typealias Index : ForwardIndex
  var startIndex: Index {get}
  var endIndex: Index {get}
  typealias _Element
  subscript(_i: Index) -> _Element {get}
}

protocol Collection : Indexable, Sequence {}

public struct IndexingIterator<Elements : Indexable>
  : IteratorProtocol, Sequence {
  
  public func makeIterator() -> IndexingIterator {
    return self
  }
  
  public mutating func next() -> Elements._Element? {
    return nil
  }
}

extension Sequence where Self : Collection {
  func makeIterator() -> IndexingIterator<Self> {
    return IndexingIterator(self)
  }
}

