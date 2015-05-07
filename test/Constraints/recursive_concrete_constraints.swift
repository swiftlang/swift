// RUN: %target-parse-verify-swift

struct S<A: CollectionType where A.Index == Int> : CollectionType {
  typealias Element = A.Generator.Element
  typealias Index = A.Index
  
  init(base: A, baseRange: Range<Index>) {
    self.base = base
    self.baseRange = baseRange
  }
  
  var startIndex: Index {
    return Int(0)
  }
  
  var endIndex: Index {
    return baseRange.count()
  }

  subscript(i: Index) -> Element {
    return base[baseRange.startIndex + i]
  }
  
  func generate() -> IndexingGenerator<S> {
    return IndexingGenerator(self)
  }
  
  var base: A
  var baseRange: Range<A.Index>
}
