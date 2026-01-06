// RUN: %target-typecheck-verify-swift -swift-version 5 -disable-objc-attr-requires-foundation-module -enable-objc-interop -package-name myPkg
// RUN: %target-typecheck-verify-swift -swift-version 5 -disable-objc-attr-requires-foundation-module -enable-objc-interop -enable-testing -package-name myPkg

@usableFromInline private func privateVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but global function 'privateVersioned()' is private}}

@usableFromInline fileprivate func fileprivateVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but global function 'fileprivateVersioned()' is fileprivate}}

@usableFromInline internal func internalVersioned() {}
// OK

@usableFromInline package func packageVersioned() {}
// OK

@usableFromInline func implicitInternalVersioned() {}
// OK

@usableFromInline public func publicVersioned() {}
// expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but global function 'publicVersioned()' is public}}

// expected-note@+1 3{{global function 'internalFunc()' is not '@usableFromInline' or public}}
internal func internalFunc() {}
// expected-note@+1 3{{global function 'packageFunc()' is not '@usableFromInline' or public}}
package func packageFunc() {}
public func publicFunc() {}

@inlinable
public func publicInlinableFunc() {
  internalVersioned() // OK
  internalFunc() // expected-error {{global function 'internalFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  packageVersioned() // OK
  packageFunc() // expected-error {{global function 'packageFunc()' is package and cannot be referenced from an '@inlinable' function}}
  publicFunc() // OK
}

@inlinable
func internalInlinableFunc() {
  internalVersioned() // OK
  internalFunc() // expected-error {{global function 'internalFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  packageVersioned() // OK
  packageFunc() // expected-error {{global function 'packageFunc()' is package and cannot be referenced from an '@inlinable' function}}
  publicFunc() // OK
}

@inlinable
package func packageInlinableFunc() {
  internalVersioned() // OK
  internalFunc() // expected-error {{global function 'internalFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  packageVersioned() // OK
  packageFunc() // expected-error {{global function 'packageFunc()' is package and cannot be referenced from an '@inlinable' function}}
  publicFunc() // OK
}

package class PackageClass {
  // expected-note@-1 *{{type declared here}}
  @usableFromInline public func publicVersioned() {}
  // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but instance method 'publicVersioned()' is public}}
}

internal class InternalClass {
  // expected-note@-1 2{{type declared here}}
  @usableFromInline public func publicVersioned() {}
  // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but instance method 'publicVersioned()' is public}}
}

fileprivate class filePrivateClass {
  @usableFromInline internal func internalVersioned() {}
  @usableFromInline package func packageVersioned() {}
}

@usableFromInline struct S {
  var x: Int
  @usableFromInline var y: Int
}

@usableFromInline extension S {}
// expected-error@-1 {{'@usableFromInline' attribute cannot be applied to this declaration}}

@usableFromInline package struct Pkg {
  var x: Int
  @usableFromInline var y: Int
  @usableFromInline package var z: Int
}

@usableFromInline extension Pkg {}
// expected-error@-1 {{'@usableFromInline' attribute cannot be applied to this declaration}}

@usableFromInline
protocol VersionedProtocol {
  associatedtype T

  func requirement() -> T

  public func publicRequirement() -> T
  // expected-error@-1 {{'public' modifier cannot be used in protocols}}
  // expected-note@-2 {{protocol requirements implicitly have the same access as the protocol itself}}

  @usableFromInline func versionedRequirement() -> T
  // expected-error@-1 {{'@usableFromInline' attribute cannot be used in protocols}}
}

@usableFromInline
package protocol PkgVersionedProtocol {
  associatedtype T

  func requirement() -> T

  public func publicRequirement() -> T
  // expected-error@-1 {{'public' modifier cannot be used in protocols}}
  // expected-note@-2 {{protocol requirements implicitly have the same access as the protocol itself}}

  @usableFromInline func versionedRequirement() -> T
  // expected-error@-1 {{'@usableFromInline' attribute cannot be used in protocols}}
}

// Derived conformances had issues with @usableFromInline - rdar://problem/34342955
@usableFromInline
internal enum EqEnum {
  case foo
}
@usableFromInline
package enum PkgEqEnum {
  case foo
}

@usableFromInline
internal enum RawEnum : Int {
  case foo = 0
}
@usableFromInline
package enum PkgRawEnum : Int {
  case foo = 0
}

@inlinable
public func usesEqEnum() -> Bool {
  _ = (EqEnum.foo == .foo)
  _ = EqEnum.foo.hashValue

  _ = RawEnum.foo.rawValue
  _ = RawEnum(rawValue: 0)

  _ = (PkgEqEnum.foo == .foo)
  _ = PkgEqEnum.foo.hashValue

  _ = PkgRawEnum.foo.rawValue
  _ = PkgRawEnum(rawValue: 0)
}

public class DynamicMembers {
  @usableFromInline @objc dynamic init() {}
  @usableFromInline @objc dynamic func foo() {}
  @usableFromInline @objc dynamic var bar: Int = 0
  @usableFromInline @objc dynamic package init(arg: Int) {}
  @usableFromInline @objc dynamic package func baz() {}
  @usableFromInline @objc dynamic package var cat: Int = 0
}

internal struct InternalStruct {}
// expected-note@-1 9{{type declared here}}

@usableFromInline var globalInferred = InternalStruct()
// expected-error@-1 {{type referenced from a '@usableFromInline' variable with inferred type 'InternalStruct' must be '@usableFromInline' or public}}

@usableFromInline var globalDeclared: InternalStruct = InternalStruct()
// expected-error@-1 {{type referenced from a '@usableFromInline' variable must be '@usableFromInline' or public}}

@usableFromInline typealias BadAlias = InternalStruct
// expected-error@-1 {{type referenced from the underlying type of a '@usableFromInline' type alias must be '@usableFromInline' or public}}

package struct PackageStruct {}
// expected-note@-1 *{{type declared here}}

@usableFromInline var globalInferredPkg = PackageStruct()
// expected-error@-1 {{type referenced from a '@usableFromInline' variable with inferred type 'PackageStruct' must be '@usableFromInline' or public}}

@usableFromInline var globalDeclaredPkg: PackageStruct = PackageStruct()
// expected-error@-1 {{type referenced from a '@usableFromInline' variable must be '@usableFromInline' or public}}

@usableFromInline typealias BadAliasPkg = PackageStruct
// expected-error@-1 {{type referenced from the underlying type of a '@usableFromInline' type alias must be '@usableFromInline' or public}}


protocol InternalProtocol {
  // expected-note@-1 * {{type declared here}}
  associatedtype T
}

@usableFromInline
struct BadStruct<T, U>
// expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' generic struct must be '@usableFromInline' or public}}
where T : InternalProtocol,
      T : Sequence,
      T.Element == InternalStruct {
  @usableFromInline init(x: InternalStruct) {}
  // expected-error@-1 {{the parameter of a '@usableFromInline' initializer must be '@usableFromInline' or public}}

  @usableFromInline func foo(x: InternalStruct) -> InternalClass {}
  // expected-error@-1 {{the parameter of a '@usableFromInline' method must be '@usableFromInline' or public}}
  // expected-error@-2 {{the result of a '@usableFromInline' method must be '@usableFromInline' or public}}

  @usableFromInline var propertyInferred = InternalStruct()
  // expected-error@-1 {{type referenced from a '@usableFromInline' property with inferred type 'InternalStruct' must be '@usableFromInline' or public}}

  @usableFromInline var propertyDeclared: InternalStruct = InternalStruct()
  // expected-error@-1 {{type referenced from a '@usableFromInline' property must be '@usableFromInline' or public}}

  @usableFromInline subscript(x: InternalStruct) -> Int {
    // expected-error@-1 {{index type of a '@usableFromInline' subscript must be '@usableFromInline' or public}}
    get {}
    set {}
  }

  @usableFromInline subscript(x: Int) -> InternalStruct {
    // expected-error@-1 {{element type of a '@usableFromInline' subscript must be '@usableFromInline' or public}}
    get {}
    set {}
  }
}

@usableFromInline
protocol BadProtocol : InternalProtocol {
  // expected-error@-1 {{protocol refined by '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype X : InternalProtocol
  // expected-error@-1 {{type referenced from a requirement of an associated type in a '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype Y = InternalStruct
  // expected-error@-1 {{type referenced from a default definition of an associated type in a '@usableFromInline' protocol must be '@usableFromInline' or public}}
}

@usableFromInline
protocol AnotherBadProtocol where Self.T : InternalProtocol {
  // expected-error@-1 {{protocol used by '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype T
}


package protocol PackageProtocol {
  // expected-note@-1 * {{type declared here}}
  associatedtype T
}

@usableFromInline
package struct PkgBadStruct<T, U>
// expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' generic struct must be '@usableFromInline' or public}}
where T : PackageProtocol,
      T : Sequence,
      T.Element == PackageStruct {
  @usableFromInline init(x: PackageStruct) {}
  // expected-error@-1 {{the parameter of a '@usableFromInline' initializer must be '@usableFromInline' or public}}

  @usableFromInline func foo(x: PackageStruct) -> PackageClass {}
  // expected-error@-1 {{the parameter of a '@usableFromInline' method must be '@usableFromInline' or public}}
  // expected-error@-2 {{the result of a '@usableFromInline' method must be '@usableFromInline' or public}}

  @usableFromInline var propertyInferred = PackageStruct()
  // expected-error@-1 {{type referenced from a '@usableFromInline' property with inferred type 'PackageStruct' must be '@usableFromInline' or public}}

  @usableFromInline var propertyDeclared: PackageStruct = PackageStruct()
  // expected-error@-1 {{type referenced from a '@usableFromInline' property must be '@usableFromInline' or public}}

  @usableFromInline subscript(x: PackageStruct) -> Int {
    // expected-error@-1 {{index type of a '@usableFromInline' subscript must be '@usableFromInline' or public}}
    get {}
    set {}
  }

  @usableFromInline subscript(x: Int) -> PackageStruct {
    // expected-error@-1 {{element type of a '@usableFromInline' subscript must be '@usableFromInline' or public}}
    get {}
    set {}
  }
}

@usableFromInline
package protocol PkgBadProtocol : PackageProtocol {
  // expected-error@-1 {{protocol refined by '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype X : PackageProtocol
  // expected-error@-1 {{type referenced from a requirement of an associated type in a '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype Y = PackageProtocol
  // expected-error@-1 {{type referenced from a default definition of an associated type in a '@usableFromInline' protocol must be '@usableFromInline' or public}}
}

@usableFromInline
package protocol PkgAnotherBadProtocol where Self.T : PackageProtocol {
  // expected-error@-1 {{protocol used by '@usableFromInline' protocol must be '@usableFromInline' or public}}
  associatedtype T
}


@usableFromInline
enum BadEnum {
  case bad(InternalStruct)
  // expected-error@-1 {{type of enum case in '@usableFromInline' enum must be '@usableFromInline' or public}}
}

@usableFromInline
package enum PkgBadEnum {
  case bad(PackageStruct)
  // expected-error@-1 {{type of enum case in '@usableFromInline' enum must be '@usableFromInline' or public}}
}

@usableFromInline
class BadClass : InternalClass {}
// expected-error@-1 {{type referenced from the superclass of a '@usableFromInline' class must be '@usableFromInline' or public}}

@usableFromInline
package class PkgBadClass : PackageClass {}
// expected-error@-1 {{type referenced from the superclass of a '@usableFromInline' class must be '@usableFromInline' or public}}

public struct TestGenericSubscripts {
  @usableFromInline subscript<T: InternalProtocol>(_: T) -> Int { return 0 } // expected-warning {{type referenced from a generic parameter of a '@usableFromInline' subscript should be '@usableFromInline' or public}}
  @usableFromInline subscript<T>(where _: T) -> Int where T: InternalProtocol { return 0 } // expected-warning {{type referenced from a generic requirement of a '@usableFromInline' subscript should be '@usableFromInline' or public}}
  @usableFromInline package subscript<T: PackageProtocol>(_: T) -> Int { return 0 } // expected-warning {{type referenced from a generic parameter of a '@usableFromInline' subscript should be '@usableFromInline' or public}}
  @usableFromInline package subscript<T>(where _: T) -> Int where T: PackageProtocol { return 0 } // expected-warning {{type referenced from a generic requirement of a '@usableFromInline' subscript should be '@usableFromInline' or public}}
}

@usableFromInline typealias TestGenericAlias<T: InternalProtocol> = T // expected-warning {{type referenced from a generic parameter of a '@usableFromInline' generic type alias should be '@usableFromInline' or public}}
@usableFromInline typealias TestGenericAliasWhereClause<T> = T where T: InternalProtocol // expected-warning {{type referenced from a generic requirement of a '@usableFromInline' generic type alias should be '@usableFromInline' or public}}

@usableFromInline typealias PkgTestGenericAlias<T: PackageProtocol> = T // expected-warning {{type referenced from a generic parameter of a '@usableFromInline' generic type alias should be '@usableFromInline' or public}}
@usableFromInline typealias PkgTestGenericAliasWhereClause<T> = T where T: PackageProtocol // expected-warning {{type referenced from a generic requirement of a '@usableFromInline' generic type alias should be '@usableFromInline' or public}}

@usableFromInline struct GenericStruct<T> {
  @usableFromInline struct Nested where T : InternalProtocol {}
  // expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' struct must be '@usableFromInline' or public}}

  @usableFromInline func nonGenericWhereClause() where T : InternalProtocol {}
  // expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' instance method must be '@usableFromInline' or public}}

  @usableFromInline package struct PkgNested where T : PackageProtocol {}
  // expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' struct must be '@usableFromInline' or public}}

  @usableFromInline package func pkgNonGenericWhereClause() where T : PackageProtocol {}
  // expected-error@-1 {{type referenced from a generic requirement of a '@usableFromInline' instance method must be '@usableFromInline' or public}}
}

public struct IncorrectInitUse {
  public var x: Int {
    @usableFromInline
    // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but getter for property 'x' is public}}
    get { 0 }

    @usableFromInline
    // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but setter for property 'x' is public}}
    set { }
  }

  @usableFromInline
  // expected-error@-1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but initializer 'init(x:)' is public}}
  public init(x: Int) {
    self.x = x
  }
}
