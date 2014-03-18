// RUN: %swift -parse -verify %s

struct S<A: Collection where A.IndexType == Int> : Collection {
  typealias Element = A.GeneratorType.Element
  typealias IndexType = A.IndexType
  
  init(base: A, baseRange: Range<IndexType>) {
    self.base = base
    self.baseRange = baseRange
  }
  
  var startIndex: IndexType {
    return .from(0)
  }
  
  var endIndex: IndexType {
    return Swift.count(baseRange)
  }

  subscript(i: IndexType) -> Element {
    return base[baseRange.startIndex + i]
  }
  
  func generate() -> IndexingGenerator<S> {
    return IndexingGenerator(self)
  }
  
  var base: A
  var baseRange: Range<A.IndexType>
}
