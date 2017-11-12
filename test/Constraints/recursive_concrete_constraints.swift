// RUN: %target-typecheck-verify-swift

struct S<A: Collection> : Collection where A.Index == Int {
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

  func index(after i: Index) -> Index {
    return base.index(after: i)
  }
  
  subscript(i: Index) -> Element {
    return base[baseRange.lowerBound + i]
  }
  
  func makeIterator() -> IndexingIterator<S> {
    return IndexingIterator(_elements: self)
  }
  
  var base: A
  var baseRange: Range<A.Index>
}
