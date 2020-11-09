// RUN: not %target-swift-frontend -typecheck %s

struct CountSteps1<T> : Collection {
  init(count: Int) { self.count = count }
  var count: Int

  var startIndex: Int { 0 }
  var endIndex: Int { count }
  func index(after i: Int) -> Int { 
    totalSteps += 1
    return i + 1
  }
  subscript(i: Int) -> Int { return i }
}

extension CountSteps1
  : RandomAccessCollection, BidirectionalCollection 
     where T : Equatable 
{
  func index(_ i: Index, offsetBy d: Int) -> Index {
    return i + d
  }
}
