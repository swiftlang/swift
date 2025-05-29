public protocol ResilientProto {
  associatedtype T
  func impl()
}


public struct ResilientStruct<T> : ResilientProto {
  var x : T?
  public init(_ t: T) {
      x = t
  }
  public func impl() {
    print(x)
  }
}
