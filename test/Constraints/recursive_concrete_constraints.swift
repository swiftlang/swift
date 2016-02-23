// RUN: %target-parse-verify-swift

struct S<A: Collection where A.Index == Int> : Collection {
  typealias Element = A.Iterator.Element
  typealias Index = A.Index
  
  init(base: A, baseRange: Range<Index>) {
    self.base = base
    self.baseRange = baseRange
  }
  
  var startIndex: Index {
    return Int(0)
  }
  
  var endIndex: Index {
    return baseRange.count
  }

  subscript(i: Index) -> Element {
    return base[baseRange.startIndex + i]
  }
  
  func makeIterator() -> IndexingIterator<S> {
    return IndexingIterator(_elements: self)
  }
  
  var base: A
  var baseRange: Range<A.Index>
}
