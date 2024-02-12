// RUN: %target-typecheck-verify-swift -swift-version 5 -package-name myPkg

private struct PrivateStruct {} // expected-note 10{{type declared here}}
internal struct InternalStruct {} // expected-note 4{{type declared here}}
package struct PackageStruct {} // expected-note *{{type declared here}}
public struct PublicStruct {}
private class PrivateClass {} // expected-note 4{{type declared here}}

public protocol BaseProtocol {
  associatedtype T
}

public protocol BaseProtocol2 {}
private typealias PrivateTypeAlias = BaseProtocol2 // expected-note 2{{type declared here}}

public protocol PublicProtocol1 : BaseProtocol where T == PrivateStruct {
  // expected-error@-1 {{public protocol's 'where' clause cannot use a private struct}}

  associatedtype X : BaseProtocol where X.T == PrivateStruct
  // expected-error@-1 {{associated type in a public protocol uses a private type in its requirement}}
}

public protocol PublicProtocol2 : BaseProtocol where T == InternalStruct {
  // expected-error@-1 {{public protocol's 'where' clause cannot use an internal struct}}

  associatedtype X : BaseProtocol where X.T == InternalStruct
  // expected-error@-1 {{associated type in a public protocol uses an internal type in its requirement}}
}

public protocol PublicProtocol3 : BaseProtocol where T == PackageStruct {
  // expected-error@-1 {{public protocol's 'where' clause cannot use a package struct}}
  associatedtype X : BaseProtocol where X.T == PackageStruct
  // expected-error@-1 {{associated type in a public protocol uses a package type in its requirement}}
}

public protocol PublicProtocol4 : BaseProtocol where T == PublicStruct {
  associatedtype X : BaseProtocol where X.T == PublicStruct
}

package protocol PackageProtocol1 : BaseProtocol where T == PrivateStruct {
  // expected-error@-1 {{package protocol's 'where' clause cannot use a private struct}}

  associatedtype X : BaseProtocol where X.T == PrivateStruct
  // expected-error@-1 {{associated type in a package protocol uses a private type in its requirement}}
}

package protocol PackageProtocol2 : BaseProtocol where T == InternalStruct {
  // expected-error@-1 {{package protocol's 'where' clause cannot use an internal struct}}

  associatedtype X : BaseProtocol where X.T == InternalStruct
  // expected-error@-1 {{associated type in a package protocol uses an internal type in its requirement}}
}

package protocol PackageProtocol3 : BaseProtocol where T == PackageStruct {
  associatedtype X : BaseProtocol where X.T == PackageStruct
}

package protocol PackageProtocol4 : BaseProtocol where T == PublicStruct {
  associatedtype X : BaseProtocol where X.T == PublicStruct
}

internal protocol InternalProtocol1 : BaseProtocol where T == PrivateStruct {
  // expected-error@-1 {{internal protocol's 'where' clause cannot use a private struct}}

  associatedtype X : BaseProtocol where X.T == PrivateStruct
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

internal protocol InternalProtocol2 : BaseProtocol where T == InternalStruct {
  associatedtype X : BaseProtocol where X.T == InternalStruct
}

internal protocol InternalProtocol3 : BaseProtocol where T == PackageStruct {
  associatedtype X : BaseProtocol where X.T == PackageStruct
}

internal protocol InternalProtocol4 : BaseProtocol where T == PublicStruct {
  associatedtype X : BaseProtocol where X.T == PublicStruct
}

protocol Protocol1 : BaseProtocol where T == PrivateStruct {
  // expected-error@-1 {{protocol must be declared private or fileprivate because its 'where' clause uses a private struct}}

  associatedtype X : BaseProtocol where X.T == PrivateStruct
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

protocol Protocol2 : BaseProtocol where T == InternalStruct {
  associatedtype X : BaseProtocol where X.T == InternalStruct
}

protocol Protocol3 : BaseProtocol where T == PublicStruct {
  associatedtype X : BaseProtocol where X.T == PublicStruct
}

protocol Protocol4 : BaseProtocol where T == PrivateClass {
  // expected-error@-1 {{protocol must be declared private or fileprivate because its 'where' clause uses a private class}}

  associatedtype X : BaseProtocol where X.T == PrivateClass
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

protocol Protocol5 : BaseProtocol where T == PrivateTypeAlias {
  // expected-error@-1 {{protocol must be declared private or fileprivate because its 'where' clause uses a private type alias}}

  associatedtype X : BaseProtocol where X.T == PrivateTypeAlias
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

protocol Protocol6 : BaseProtocol where T == (PrivateClass, AnyObject) {
  // expected-error@-1 {{protocol must be declared private or fileprivate because its 'where' clause uses a private type}}

  associatedtype X : BaseProtocol where X.T == (PrivateClass, AnyObject)
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

protocol Protocol7 : BaseProtocol where T == (PrivateStruct) -> Void {
  // expected-error@-1 {{protocol must be declared private or fileprivate because its 'where' clause uses a private type}}

  associatedtype X : BaseProtocol where X.T == (PrivateStruct) -> Void
  // expected-error@-1 {{associated type in an internal protocol uses a private type in its requirement}}
}

private protocol PrivateProtocol {} // expected-note 4{{type declared here}}

struct GenericStruct<T> {
  struct Inner where T : PrivateProtocol {}
  // expected-error@-1 {{struct must be declared private because its generic requirement uses a private type}}
  func nonGenericWhereClause() where T : PrivateProtocol {}
  // expected-error@-1 {{instance method must be declared private because its generic requirement uses a private type}}
}

package struct PkgGenericStruct<T> {
  package struct Inner where T : PrivateProtocol {}
  // expected-error@-1 {{struct cannot be declared package because its generic requirement uses a private type}}
  package func nonGenericWhereClause() where T : PrivateProtocol {}
  // expected-error@-1 {{instance method cannot be declared package because its generic requirement uses a private type}}
}
