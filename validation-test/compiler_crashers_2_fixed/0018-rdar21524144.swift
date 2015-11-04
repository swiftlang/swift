// RUN: not %target-swift-frontend %s -parse

public protocol Indexable {
  typealias Index : ForwardIndex
  var startIndex: Index {get}
  var endIndex: Index {get}
  typealias _Element
  subscript(_i: Index) -> _Element {get}
}

protocol Collection : Indexable, Sequence {}

public struct CollectionDefaultIterator<Elements : Indexable>
  : IteratorProtocol, Sequence {
  
  public func iterator() -> CollectionDefaultIterator {
    return self
  }
  
  public mutating func next() -> Elements._Element? {
    return nil
  }
}

extension Sequence where Self : Collection {
  func iterator() -> CollectionDefaultIterator<Self> {
    return CollectionDefaultIterator(self)
  }
}

