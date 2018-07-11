public protocol Proto {
  func wellNow()
}


public struct Impl {
  var x: Int
  public init() {
    x = 9
  }
}

public struct Container<T: Proto> {
  public var contained : T
  public init(_ i: T) {
    contained = i
  }
}

public var any : Any = 0
