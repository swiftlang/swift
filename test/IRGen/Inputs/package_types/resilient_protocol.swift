
package protocol OtherResilientProtocol {
}

var x: Int = 0

extension OtherResilientProtocol {
  package var propertyInExtension: Int {
    get { return x }
    set { x = newValue }
  }

  package static var staticPropertyInExtension: Int {
    get { return x }
    set { x = newValue }
  }
}

package protocol ResilientBaseProtocol {
  func requirement() -> Int
}

package protocol ResilientDerivedProtocol : ResilientBaseProtocol {}

package protocol ProtocolWithRequirements {
  associatedtype T
  func first()
  func second()
}

package struct Wrapper<T>: OtherResilientProtocol { }

package struct ConcreteWrapper: OtherResilientProtocol { }

package protocol ProtocolWithAssocTypeDefaults {
  associatedtype T1 = Self
  associatedtype T2: OtherResilientProtocol = Wrapper<T1>
}

package protocol ResilientSelfDefault : ResilientBaseProtocol {
  associatedtype AssocType: ResilientBaseProtocol = Self
}

@_fixed_layout package protocol OtherFrozenProtocol {
  func protocolMethod()
}
