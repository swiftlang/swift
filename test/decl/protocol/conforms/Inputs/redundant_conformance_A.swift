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

