
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

public struct ConcreteWrapper: OtherResilientProtocol { }

public protocol ProtocolWithAssocTypeDefaults {
  associatedtype T1 = Self
  associatedtype T2: OtherResilientProtocol = Wrapper<T1>
}

public protocol ResilientSelfDefault : ResilientBaseProtocol {
  associatedtype AssocType: ResilientBaseProtocol = Self
}

@_fixed_layout public protocol OtherFrozenProtocol {
  func protocolMethod()
}

@available(SwiftStdlib 5.10, *)
public protocol ResilientSendableBase: Sendable {
  func f()
}

@available(SwiftStdlib 5.10, *)
public protocol ResilientSendable: ResilientSendableBase {
  func g()
}

@available(SwiftStdlib 5.10, *)
public struct ConformsToResilientSendable: ResilientSendable {
  public func f() { }
  public func g() { }
}

@available(SwiftStdlib 6.0, *)
public protocol NewResilientSendableBase: Sendable {
  func f()
}

@available(SwiftStdlib 6.0, *)
public protocol NewResilientSendable: NewResilientSendableBase {
  func g()
}

@available(SwiftStdlib 6.0, *)
public struct ConformsToNewResilientSendable: NewResilientSendable {
  public func f() { }
  public func g() { }
}
