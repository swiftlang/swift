// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

// https://github.com/apple/swift/issues/56782

public struct MyList: Sequence {
  var _list: [(Int, Int)]
  
  public func makeIterator() -> some IteratorProtocol {
    return _list.makeIterator()
  }
}
