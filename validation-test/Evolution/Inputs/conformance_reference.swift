
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}


public protocol BaseProtocol {
#if AFTER
  associatedtype Assoc = Self
#endif
}

public protocol DerivedProtocol : BaseProtocol {}


public struct FirstGeneric<T> : BaseProtocol {
  public init() {}
}

public struct SecondGeneric<T> : DerivedProtocol {
  public init() {}
}

extension BaseProtocol {
  public func getMeAType() -> Any.Type {
#if BEFORE
    return Self.self
#else
    return Assoc.self
#endif
  }
}