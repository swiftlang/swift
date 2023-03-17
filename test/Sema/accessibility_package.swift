// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Utils %t/Utils.swift -emit-module -emit-module-path %t/Utils.swiftmodule -package-name myLib
// RUN: test -f %t/Utils.swiftmodule

// RUN: %target-swift-frontend -module-name LibGood %t/LibGood.swift -emit-module -emit-module-path %t/LibGood.swiftmodule -package-name myLib -I %t
// RUN: test -f %t/LibGood.swiftmodule

// RUN: not %target-swift-frontend -module-name Client %t/Client.swift -emit-module -emit-module-path %t/Client.swiftmodule -package-name client -I %t 2> %t/resultClient.output
// RUN: %FileCheck %s -input-file %t/resultClient.output -check-prefix CHECK-CLIENT
// CHECK-CLIENT: error: 'pkgVar' is inaccessible due to 'package' protection level
// CHECK-CLIENT: error: 'pkgFunc' is inaccessible due to 'package' protection level

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -package-name myLib -I %t 2> %t/result1.output
// RUN: %FileCheck %s -input-file %t/result1.output -check-prefix CHECK-1
// CHECK-1: error: overriding non-open instance method outside of its defining module
// CHECK-1: error: overriding non-open instance method outside of its defining module
// CHECK-1: error: cannot inherit from non-open class 'PublicKlass' outside of its defining module
// CHECK-1: error: cannot inherit from non-open class 'PackageKlass' outside of its defining module
// CHECK-1: error: cannot assign to property: 'publicGetInternal' setter is inaccessible
// CHECK-1: error: cannot assign to property: 'pkgVar' setter is inaccessible

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -package-name "otherLib" -I %t 2> %t/result2.output
// %FileCheck %s -input-file %t/result2.output -check-prefix CHECK-2
// CHECK-2: error: cannot find type 'PackageProto' in scope
// CHECK-2: error: 'pkgFunc' is inaccessible due to 'package' protection level
// CHECK-2: error: cannot find 'PackageKlass' in scope


// BEGIN Utils.swift
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

// BEGIN Lib.swift
import Utils

// Test accessing public and package decls
public func test() {
  let x = PublicKlass()
  x.publicFunc()
  x.pkgFunc() // Allowed if in same package
  x.publicGetPkg = 3 // Allowed if in same package
  x.publicGetInternal = 4 // Not allowed

  let y = PackageKlass() // Allowed if in same package
  y.pkgVar = 1.5 // Not allowed
  y.pkgFunc() // Allowed if in same package
}

// Test conformance to a package protocol
package struct LibStruct : PackageProto { // Allowed if in same package
  package var pkgVar: Double = 1.0
  package func pkgFunc() {}
}

// Test subclassing / overrides
class SubOpenKlass: OpenKlass {
  override open func openFunc() {}
  override public func publicFunc() {}
  override package func packageFunc() {}
}
class SubPublicKlass: PublicKlass {} // Not allowed
class SubPackageKlass: PackageKlass {} // Not allowed



// BEGIN LibGood.swift
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

// BEGIN Client.swift
import LibGood

func clientFunc() {
  let lib = LibStruct()
  _ = lib.pkgVar // Not allowed
  _ = lib.publicVar
  lib.pkgFunc() // Not allowed
  lib.publicFunc()
}
