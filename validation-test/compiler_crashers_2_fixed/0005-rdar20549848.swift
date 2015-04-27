// RUN: %target-swift-frontend %s -emit-silgen

public protocol MyCollectionType {
  typealias Index

  var startIndex: Index {get}
}

public struct MyMinimalBidirectionalIndex : BidirectionalIndexType {
  public func successor() -> MyMinimalBidirectionalIndex {
    return MyMinimalBidirectionalIndex()
  }

  public func predecessor() -> MyMinimalBidirectionalIndex {
    return MyMinimalBidirectionalIndex()
  }
}

public func == (
  lhs: MyMinimalBidirectionalIndex,
  rhs: MyMinimalBidirectionalIndex
) -> Bool {
  return true
}

public struct MyMinimalBidirectionalCollection : MyCollectionType {
  public init() {
  }

  public var startIndex: MyMinimalBidirectionalIndex {
    return MyMinimalBidirectionalIndex()
  }
}

extension MyCollectionType where Self.Index : BidirectionalIndexType {
  final public var my_prext_isEmpty: Bool {
	let i = startIndex
    return i == i
  }
}


let s = MyMinimalBidirectionalCollection()
s.my_prext_isEmpty

