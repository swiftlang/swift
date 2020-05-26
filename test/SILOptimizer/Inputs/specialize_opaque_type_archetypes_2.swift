public protocol ExternalP {
  func myValue2() -> Int64
}

extension Int64 : ExternalP {
  public func myValue2() -> Int64 {
    return self + 2
  }
}

public func external() -> some ExternalP {
  return Int64(5)
}

public struct ExternalContainer {
  var x = Int64(0)

  public init() {}

  public var computedProperty : some ExternalP {
    return x
  }
}
