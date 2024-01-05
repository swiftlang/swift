// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Utils1 %t/Utils.swift -emit-module -emit-module-path %t/Utils1.swiftmodule -package-name myLib -swift-version 5
// RUN: test -f %t/Utils1.swiftmodule

// RUN: %target-swift-frontend -module-name Utils %t/Utils.swift -emit-module -emit-module-interface-path %t/Utils.swiftinterface -package-name myLib -enable-library-evolution -swift-version 5
// RUN: test -f %t/Utils.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Utils.swiftinterface) -I%t

// RUN: %FileCheck %s -check-prefix CHECK-UTILS < %t/Utils.swiftinterface
// CHECK-UTILS: -module-name Utils
// CHECK-UTILS: -package-name myLib
// CHECK-UTILS: @usableFromInline
// CHECK-UTILS: package class PackageKlassProto {
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   package init()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   package var pkgVar: Swift.Double
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   deinit
// CHECK-UTILS: }
// CHECK-UTILS: @usableFromInline
// CHECK-UTILS: package class PackageKlassForInline {
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   package init()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   package func foo1()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   deinit
// CHECK-UTILS: }
// CHECK-UTILS: @usableFromInline
// CHECK-UTILS: internal class InternalKlassProto {
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   internal init()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   internal var internalVar: Swift.Double
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   deinit
// CHECK-UTILS: }
// CHECK-UTILS: @usableFromInline
// CHECK-UTILS: internal class InternalKlassForInline {
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   internal init()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   internal func bar1()
// CHECK-UTILS:   @usableFromInline
// CHECK-UTILS:   deinit
// CHECK-UTILS: }
// CHECK-UTILS: @inlinable public func publicFunc() {
// CHECK-UTILS:   let a = PackageKlassProto().pkgVar
// CHECK-UTILS:   let b = InternalKlassProto().internalVar
// CHECK-UTILS:   PackageKlassForInline().foo1()
// CHECK-UTILS:   InternalKlassForInline().bar1()
// CHECK-UTILS:   print(a, b)
// CHECK-UTILS: }
// CHECK-UTILS: @inlinable internal func internalFunc() {
// CHECK-UTILS:   let a = PackageKlassProto().pkgVar
// CHECK-UTILS:   let b = InternalKlassProto().internalVar
// CHECK-UTILS:   PackageKlassForInline().foo1()
// CHECK-UTILS:   InternalKlassForInline().bar1()
// CHECK-UTILS:   print(a, b)
// CHECK-UTILS: }
// CHECK-UTILS: @inlinable package func packageFunc() {
// CHECK-UTILS:   let a = PackageKlassProto().pkgVar
// CHECK-UTILS:   let b = InternalKlassProto().internalVar
// CHECK-UTILS:   PackageKlassForInline().foo1()
// CHECK-UTILS:   InternalKlassForInline().bar1()
// CHECK-UTILS:   print(a, b)
// CHECK-UTILS: }


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

@usableFromInline
package class PackageKlassProto: PackageProto {
  @usableFromInline package init() {}
  @usableFromInline package var pkgVar = 1.0
  package func pkgFunc() {}
}

@usableFromInline
package class PackageKlassForInline {
  @usableFromInline package init() {}
  @usableFromInline package func foo1() {}
  package func foo2() {}
}

protocol InternalProto {
  var internalVar: Double { get set }
  func internalFunc()
}
class InternalKlass {
  init() {}
}
@usableFromInline
class InternalKlassProto: InternalProto {
  @usableFromInline init() {}
  @usableFromInline var internalVar = 1.0
  func internalFunc() {}
}

@usableFromInline
class InternalKlassForInline {
  @usableFromInline init() {}
  @usableFromInline func bar1() {}
  func bar2() {}
}

@inlinable
public func publicFunc() {
  let a = PackageKlassProto().pkgVar
  let b = InternalKlassProto().internalVar
  PackageKlassForInline().foo1()
  InternalKlassForInline().bar1()
  print(a, b)
}

@inlinable
func internalFunc() {
  let a = PackageKlassProto().pkgVar
  let b = InternalKlassProto().internalVar
  PackageKlassForInline().foo1()
  InternalKlassForInline().bar1()
  print(a, b)
}

@inlinable
package func packageFunc() {
  let a = PackageKlassProto().pkgVar
  let b = InternalKlassProto().internalVar
  PackageKlassForInline().foo1()
  InternalKlassForInline().bar1()
  print(a, b)
}
