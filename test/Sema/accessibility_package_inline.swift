// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name UtilsGood %t/UtilsGood.swift -emit-module -emit-module-path %t/UtilsGood.swiftmodule -package-name myLib
// RUN: test -f %t/UtilsGood.swiftmodule

// RUN: not %target-swift-frontend -typecheck %t/Utils.swift -package-name myLib -I %t 2> %t/resultUtils.output
// RUN: %FileCheck %s -input-file %t/resultUtils.output -check-prefix CHECK-UTILS
// CHECK-UTILS: error: class 'PackageKlass' is package and cannot be referenced from an '@inlinable' function
// CHECK-UTILS:   let a = PackageKlass()
// CHECK-UTILS:           ^
// CHECK-UTILS: note: class 'PackageKlass' is not '@usableFromInline' or public
// CHECK-UTILS: package class PackageKlass {
// CHECK-UTILS:               ^
// CHECK-UTILS: error: initializer 'init()' is package and cannot be referenced from an '@inlinable' function
// CHECK-UTILS:  let a = PackageKlass() // should error
// CHECK-UTILS:          ^
// CHECK-UTILS: note: initializer 'init()' is not '@usableFromInline' or public
// CHECK-UTILS:  package init() {}
// CHECK-UTILS:          ^
// CHECK-UTILS: error: class 'InternalKlass' is internal and cannot be referenced from an '@inlinable' function
// CHECK-UTILS:  let b = InternalKlass() // should error
// CHECK-UTILS:          ^
// CHECK-UTILS: note: class 'InternalKlass' is not '@usableFromInline' or public
// CHECK-UTILS: class InternalKlass {
// CHECK-UTILS:       ^
// CHECK-UTILS: error: initializer 'init()' is internal and cannot be referenced from an '@inlinable' function
// CHECK-UTILS:  let b = InternalKlass() // should error
// CHECK-UTILS:          ^
// CHECK-UTILS: note: initializer 'init()' is not '@usableFromInline' or public
// CHECK-UTILS:  init() {}
// CHECK-UTILS:  ^

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -package-name "otherLib" -I %t 2> %t/resultLib.output
// %FileCheck %s -input-file %t/resultLib.output -check-prefix CHECK-LIB
// CHECK-LIB: error: cannot find 'packageFunc' in scope
// CHECK-LIB: packageFunc()
// CHECK-LIB: ^~~~~~~~~~~

// RUN: %target-swift-frontend -module-name Lib %t/Lib.swift -emit-module -emit-module-path %t/Lib.swiftmodule -package-name myLib -I %t
// RUN: test -f %t/Lib.swiftmodule

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
  @usableFromInline
  package init() {}
  @usableFromInline
  package var pkgVar = 1.0
  package func pkgFunc() {}
}

@usableFromInline
package class PackageKlassForInline {
  @usableFromInline
  package init() {}
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
  @usableFromInline
  init() {}
  var internalVar = 1.0
  func internalFunc() {}
}

@usableFromInline
class InternalKlassForInline {
  @usableFromInline
  init() {}
}

@inlinable
public func publicFunc() {
  let a = PackageKlass() // should error
  let b = InternalKlass() // should error
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

@inlinable
func internalFunc() {
  let a = PackageKlass() // should error
  let b = InternalKlass() // should error
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

@inlinable
package func packageFunc() {
  let a = PackageKlass() // should error
  let b = InternalKlass() // should error
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

// BEGIN UtilsGood.swift
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
package class PackageKlassForInline {
  @usableFromInline
  package init() {}
}

class InternalKlass {
  init() {}
}

@usableFromInline
class InternalKlassForInline {
  @usableFromInline
  init() {}
}

@inlinable
public func publicFunc() {
  let x = PackageKlassForInline()
  let y = InternalKlassForInline()
  print(x, y)
}

@inlinable
func internalFunc() {
  let x = PackageKlassForInline()
  let y = InternalKlassForInline()
  print(x, y)
}

@inlinable
package func packageFunc() {
  let x = PackageKlassForInline()
  let y = InternalKlassForInline()
  print(x, y)
}

// BEGIN Lib.swift
import UtilsGood

@inlinable
public func libFunc() {
  publicFunc()
  packageFunc() // Allowed if in same package
}
