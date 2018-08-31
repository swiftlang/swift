import redundant_conformance_A

public protocol P2 {
  associatedtype A

  func f() -> A
}

extension OtherConformsToP: P1 {
}
extension OtherGenericConformsToP: P1 {}
extension OtherGenericConditionalConformsToP: P1 where T: P2 {}

extension ConformsToP: P2 {
  public func f() -> Int { return 0 }
}

extension GenericConformsToP: P2 {
    public func f() -> Int { return 0 }
}
extension GenericConditionalConformsToP: P2 where T: P1 {
    public func f() -> Int { return 0 }
}
