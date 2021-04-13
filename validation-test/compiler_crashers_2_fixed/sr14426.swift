// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

public struct MyList: Sequence {
  var _list: [(Int, Int)]
  
  public func makeIterator() -> some IteratorProtocol {
    return _list.makeIterator()
  }
}
