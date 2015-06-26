// RUN: not %target-swift-frontend %s -parse

public protocol Indexable {
  typealias Index : ForwardIndexType
  var startIndex: Index {get}
  var endIndex: Index {get}
  typealias _Element
  subscript(_i: Index) -> _Element {get}
}

protocol CollectionType : Indexable, SequenceType {}

public struct IndexingGenerator<Elements : Indexable>
  : GeneratorType, SequenceType {
  
  public func generate() -> IndexingGenerator {
    return self
  }
  
  public mutating func next() -> Elements._Element? {
    return nil
  }
}

extension SequenceType where Self : CollectionType {
  func generate() -> IndexingGenerator<Self> {
    return IndexingGenerator(self)
  }
}

