public protocol P1 {
  associatedtype A

  func f() -> A
}

public struct ConformsToP : P1 {
  public func f() -> Int { return 0 }
}

public struct OtherConformsToP {
  public func f() -> Int { return 0 }
}

// The downstream conformance is conditional
public struct GenericConformsToP<T> : P1 {
    public func f() -> Int { return 0 }
}

public struct OtherGenericConformsToP<T> {
    public func f() -> Int { return 0 }
}

// The upstream conformance is conditional
public struct GenericConditionalConformsToP<T> {}
extension GenericConditionalConformsToP: P1 where T == Int {
    public typealias A = Int
    public func f() -> Int { return 0 }
}

public struct OtherGenericConditionalConformsToP<T> {
    public func f() -> Int { return 0 }
}
