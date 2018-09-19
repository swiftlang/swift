
public protocol OtherResilientProtocol {
}

var x: Int = 0

extension OtherResilientProtocol {
  public var propertyInExtension: Int {
    get { return x }
    set { x = newValue }
  }

  public static var staticPropertyInExtension: Int {
    get { return x }
    set { x = newValue }
  }
}

public protocol ResilientBaseProtocol {
  func requirement() -> Int
}

public protocol ResilientDerivedProtocol : ResilientBaseProtocol {}

public protocol ProtocolWithRequirements {
  associatedtype T
  func first()
  func second()
}

public struct Wrapper<T>: OtherResilientProtocol { }

public protocol ProtocolWithAssocTypeDefaults {
  associatedtype T1 = Self
  associatedtype T2: OtherResilientProtocol = Wrapper<T1>
}

public protocol ResilientSelfDefault : ResilientBaseProtocol {
  associatedtype AssocType: ResilientBaseProtocol = Self
}
