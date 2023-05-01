// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -verify -module-name Utils %t/Utils.swift -emit-module -emit-module-path %t/Utils.swiftmodule -package-name myLib
// RUN: test -f %t/Utils.swiftmodule

// RUN: %target-swift-frontend -verify -module-name LibGood %t/LibGood.swift -emit-module -emit-module-path %t/LibGood.swiftmodule -package-name myLib -I %t
// RUN: test -f %t/LibGood.swiftmodule

// RUN: %target-swift-frontend -verify -module-name Client %t/Client.swift -emit-module -emit-module-path %t/Client.swiftmodule -package-name client -I %t

// RUN: %target-swift-frontend -typecheck -verify %t/LibSamePackage.swift -package-name myLib -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/LibOtherPackage.swift -package-name "otherLib" -I %t

//--- Utils.swift
package protocol PackageProto {
  var pkgVar: Double { get set }
  func pkgFunc()
}

package class PackageKlass {
  package init() {}
  package private(set) var pkgVar: Double = 1.0
  package func pkgFunc() {}
}

class InternalKlass {}

public class PublicKlass {
  public init() {}
  public var publicVar: Int = 1
  public package(set) var publicGetPkg: Int = 2
  public internal(set) var publicGetInternal: Int = 3
  public func publicFunc() {}
  package func pkgFunc() {}
}

open class OpenKlass {
  public init() {}
  open var openVar: String = ""
  open func openFunc() {}
  public func publicFunc() {}
  package func packageFunc() {}
}

//--- LibSamePackage.swift
import Utils

// Test accessing public and package decls
public func test() {
  let x = PublicKlass()
  x.publicFunc()
  x.pkgFunc() // OK
  x.publicGetPkg = 3 // OK
  x.publicGetInternal = 4 // expected-error {{cannot assign to property: 'publicGetInternal' setter is inaccessible}}

  let y = PackageKlass() // OK
  y.pkgVar = 1.5  // expected-error {{cannot assign to property: 'pkgVar' setter is inaccessible}}
  y.pkgFunc() // OK
}

// Test conformance to a package protocol
package struct LibStruct : PackageProto { // OK
  package var pkgVar: Double = 1.0
  package func pkgFunc() {}
}

// Test subclassing / overrides
class SubOpenKlass: OpenKlass {
  override open func openFunc() {}
  override public func publicFunc() {} // expected-error {{overriding non-open instance method outside of its defining module}}
  override package func packageFunc() {} // expected-error {{overriding non-open instance method outside of its defining module}}
}
class SubPublicKlass: PublicKlass {} // expected-error {{cannot inherit from non-open class 'PublicKlass' outside of its defining module}}
class SubPackageKlass: PackageKlass {} // expected-error {{cannot inherit from non-open class 'PackageKlass' outside of its defining module}}


//--- LibOtherPackage.swift
import Utils

// Test accessing package decls
public func test() {
  let x = PublicKlass()
  x.publicFunc() // OK
  x.pkgFunc() // expected-error {{'pkgFunc' is inaccessible due to 'package' protection level}}
  let y = PackageKlass() // expected-error {{cannot find 'PackageKlass' in scope}}
}

package struct LibStruct : PackageProto {} // expected-error {{cannot find type 'PackageProto' in scope}}

//--- LibGood.swift
import Utils

public func libFunc() {
  _ = LibStruct()
}

public struct LibStruct: PackageProto {
  public init() {}
  package var pkgVar: Double = 1.0
  package func pkgFunc() {}
  public var publicVar: String = ""
  public func publicFunc() {}
}

//--- Client.swift
import LibGood

func clientFunc() {
  let lib = LibStruct()
  _ = lib.pkgVar // expected-error {{'pkgVar' is inaccessible due to 'package' protection level}}
  _ = lib.publicVar
  lib.pkgFunc() // expected-error {{'pkgFunc' is inaccessible due to 'package' protection level}}
  lib.publicFunc()
}
