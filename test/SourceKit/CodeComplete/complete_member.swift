
protocol FooProtocol {
  /// fooInstanceVar Aaa.
  /// Bbb.
  ///
  /// Ccc.
  var fooInstanceVar: Int { get }
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(_ a: Int) -> Double
  subscript(i: Int) -> Double { get }
}

func test1(_ a: FooProtocol) {
  a.
}

func testOptional1(_ a: FooProtocol?) {
  a.
}

class C {
  @available(*, unavailable) func unavail() {}
}

func test_unavail(_ a: C) {
  a.
}

class Base {
    func foo() {}
}

class Derived: Base {
    func foo() {}
}

func testOverrideUSR() {
    Derived().
}

// RUN: %sourcekitd-test -req=complete -pos=15:5 %s -- %s > %t.response
// RUN: diff -u %s.response %t.response
//
// RUN: %sourcekitd-test -req=complete -pos=19:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-OPTIONAL
// RUN: %sourcekitd-test -req=complete.open -pos=19:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-OPTIONAL-OPEN
// CHECK-OPTIONAL:     {
// CHECK-OPTIONAL:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-OPTIONAL:       key.name: "fooInstanceFunc0()",
// CHECK-OPTIONAL-LABEL:       key.sourcetext: "?.fooInstanceFunc1(<#T##a: Int##Int#>)",
// CHECK-OPTIONAL-NEXT:       key.description: "fooInstanceFunc1(a: Int)",
// CHECK-OPTIONAL-NEXT:       key.typename: "Double",
// CHECK-OPTIONAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-OPTIONAL-NEXT:       key.num_bytes_to_erase: 1,
// CHECK-OPTIONAL-NEXT:       key.associated_usrs: "s:15complete_member11FooProtocolP16fooInstanceFunc1SdSiF",
// CHECK-OPTIONAL-NEXT:       key.modulename: "complete_member"
// CHECK-OPTIONAL-NEXT:     },

// RUN: %sourcekitd-test -req=complete.open -pos=19:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-OPTIONAL-OPEN
// CHECK-OPTIONAL-OPEN-NOT:       key.description: "fooInstanceFunc1
// CHECK-OPTIONAL-OPEN:       key.description: "?.fooInstanceFunc1(a: Int)",
// CHECK-OPTIONAL-OPEN-NOT:       key.description: "fooInstanceFunc1

// RUN: %sourcekitd-test -req=complete -pos=27:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-UNAVAIL
// CHECK-UNAVAIL-NOT: key.name: "unavail()",

// RUN: %sourcekitd-test -req=complete -pos=39:15 %s -- %s | %FileCheck %s -check-prefix=CHECK-OVERRIDE_USR
// CHECK-OVERRIDE_USR:      {
// CHECK-OVERRIDE_USR:          key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-OVERRIDE_USR-NEXT:     key.name: "foo()",
// CHECK-OVERRIDE_USR-NEXT:     key.sourcetext: "foo()",
// CHECK-OVERRIDE_USR-NEXT:     key.description: "foo()",
// CHECK-OVERRIDE_USR-NEXT:     key.typename: "Void",
// CHECK-OVERRIDE_USR-NEXT:     key.context: source.codecompletion.context.thisclass,
// CHECK-OVERRIDE_USR-NEXT:     key.num_bytes_to_erase: 0,
// CHECK-OVERRIDE_USR-NEXT:     key.associated_usrs: "s:15complete_member7DerivedC3fooyyF s:15complete_member4BaseC3fooyyF",
// CHECK-OVERRIDE_USR-NEXT:     key.modulename: "complete_member"
// CHECK-OVERRIDE_USR-NEXT: }
