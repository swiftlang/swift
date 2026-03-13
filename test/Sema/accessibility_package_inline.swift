// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -verify -module-name UtilsGood %t/UtilsGood.swift -emit-module -emit-module-path %t/UtilsGood.swiftmodule -package-name myLib
// RUN: test -f %t/UtilsGood.swiftmodule

// RUN: %target-swift-frontend -typecheck -verify %t/Utils.swift -package-name myLib -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/LibOtherPackage.swift -package-name otherLib -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/LibSamePackage.swift -package-name myLib -I %t

//--- Utils.swift
package protocol PackageProto {
  var pkgVar: Double { get set }
  func pkgFunc()
}

// expected-note@+1 *{{class 'PackageKlass' is not '@usableFromInline' or public}}
package class PackageKlass {
  package init() {} // expected-note *{{initializer 'init()' is not '@usableFromInline' or public}}
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

// expected-note@+1 *{{class 'InternalKlass' is not '@usableFromInline' or public}}
class InternalKlass {
  init() {} // expected-note *{{initializer 'init()' is not '@usableFromInline' or public}}
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
  let a = PackageKlass()
  // expected-error@-1 {{class 'PackageKlass' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is package and cannot be referenced from an '@inlinable' function}}
  let b = InternalKlass() // should error
  // expected-error@-1 {{class 'InternalKlass' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

@inlinable
func internalFunc() {
  let a = PackageKlass()
  // expected-error@-1 {{class 'PackageKlass' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is package and cannot be referenced from an '@inlinable' function}}
  let b = InternalKlass()
  // expected-error@-1 {{class 'InternalKlass' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

@inlinable
package func packageFunc() {
  let a = PackageKlass()
  // expected-error@-1 {{class 'PackageKlass' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is package and cannot be referenced from an '@inlinable' function}}
  let b = InternalKlass()
  // expected-error@-1 {{class 'InternalKlass' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2 {{initializer 'init()' is internal and cannot be referenced from an '@inlinable' function}}
  let c = PackageKlassProto()
  let d = InternalKlassProto()
  let e = PackageKlassForInline()
  let f = InternalKlassForInline()
  print(a, b, c, d, e, f)
}

//--- UtilsGood.swift
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

//--- LibOtherPackage.swift
import UtilsGood

@inlinable
public func libFunc() {
  publicFunc()
  packageFunc() // expected-error {{cannot find 'packageFunc' in scope}}
}

//--- LibSamePackage.swift
import UtilsGood

@inlinable
public func libFunc() {
  publicFunc()
  packageFunc() // OK
}
