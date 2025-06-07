// RUN: %target-typecheck-verify-swift -swift-version 4 -package-name myPkg

fileprivate struct FilePrivateStruct {}
// expected-note@-1 4{{type declared here}}

private struct PrivateStruct {}
// expected-note@-1 4{{type declared here}}

internal struct InternalStruct {}
// expected-note@-1 7{{type declared here}}

package struct PackageStruct {}
// expected-note@-1 *{{type declared here}}

public protocol P {
  typealias TFP = FilePrivateStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses a fileprivate type}}

  typealias TP = PrivateStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses a private type}}

  typealias TI = InternalStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses an internal type}}

  typealias TPkg = PackageStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses a package type}}
}

extension P {
  func usesFilePrivateStructFunc(_: FilePrivateStruct) {}
  // expected-error@-1 {{method must be declared fileprivate because its parameter uses a fileprivate type}}

  typealias UsesFilePrivateStructAlias = FilePrivateStruct
  // expected-error@-1 {{type alias must be declared fileprivate because its underlying type uses a fileprivate type}}

  var usesFilePrivateStructProp: FilePrivateStruct { get { } set { } }
  // expected-error@-1 {{property must be declared fileprivate because its type uses a fileprivate type}}


  func usesPrivateStructFunc(_: PrivateStruct) {}
  // expected-error@-1 {{method must be declared fileprivate because its parameter uses a private type}}

  typealias UsesPrivateStructAlias = PrivateStruct
  // expected-error@-1 {{type alias must be declared fileprivate because its underlying type uses a private type}}

  var usesPrivateStructProp: PrivateStruct { get { } set { } }
  // expected-error@-1 {{property must be declared fileprivate because its type uses a private type}}


  public func usesInternalStruct(_: InternalStruct) {}
  // expected-error@-1 {{method cannot be declared public because its parameter uses an internal type}}

  public typealias UsesInternalStructAlias = InternalStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses an internal type}}

  public var usesInternalStructProp: InternalStruct { get { } set { } }
  // expected-error@-1 {{property cannot be declared public because its type uses an internal type}}


  package func pkgUsesInternalStruct(_: InternalStruct) {}
  // expected-error@-1 {{method cannot be declared package because its parameter uses an internal type}}

  package typealias PkgUsesInternalStructAlias = InternalStruct
  // expected-error@-1 {{type alias cannot be declared package because its underlying type uses an internal type}}

  package var pkgUsesInternalStructProp: InternalStruct { get { } set { } }
  // expected-error@-1 {{property cannot be declared package because its type uses an internal type}}


  public func usesPackageStruct(_: PackageStruct) {}
  // expected-error@-1 {{method cannot be declared public because its parameter uses a package type}}

  public typealias UsesPackageStructAlias = PackageStruct
  // expected-error@-1 {{type alias cannot be declared public because its underlying type uses a package type}}

  public var usesPackageStructProp: PackageStruct { get { } set { } }
  // expected-error@-1 {{property cannot be declared public because its type uses a package type}}
}
