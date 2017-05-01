import redundant_conformance_A

public protocol P2 {
  associatedtype A

  func f() -> A
}

extension OtherConformsToP: P1 {
}

extension ConformsToP: P2 {
  public func f() -> Int { return 0 }
}
