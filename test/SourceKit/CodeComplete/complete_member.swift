
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
    override func foo() {}
}

func testOverrideUSR() {
    Derived().
}

// RUN: %sourcekitd-test -req=complete -pos=15:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-NORMAL
// CHECK-NORMAL:      {
// CHECK-NORMAL-NEXT:   key.results: [
// CHECK-NORMAL-NEXT:     {
// CHECK-NORMAL-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NORMAL-NEXT:       key.name: "fooInstanceFunc0()",
// CHECK-NORMAL-NEXT:       key.description: "fooInstanceFunc0()",
// CHECK-NORMAL-NEXT:       key.typename: "Double",
// CHECK-NORMAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NORMAL-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NORMAL-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NORMAL-NEXT:       key.associated_usrs: "s:15complete_member11FooProtocolP16fooInstanceFunc0SdyF",
// CHECK-NORMAL-NEXT:       key.modulename: "complete_member",
// CHECK-NORMAL-NEXT:       key.sourcetext: "fooInstanceFunc0()"
// CHECK-NORMAL-NEXT:     },
// CHECK-NORMAL-NEXT:     {
// CHECK-NORMAL-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NORMAL-NEXT:       key.name: "fooInstanceFunc1(:)",
// CHECK-NORMAL-NEXT:       key.description: "fooInstanceFunc1(a: Int)",
// CHECK-NORMAL-NEXT:       key.typename: "Double",
// CHECK-NORMAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NORMAL-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NORMAL-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NORMAL-NEXT:       key.associated_usrs: "s:15complete_member11FooProtocolP16fooInstanceFunc1ySdSiF",
// CHECK-NORMAL-NEXT:       key.modulename: "complete_member",
// CHECK-NORMAL-NEXT:       key.sourcetext: "fooInstanceFunc1(<#T##a: Int##Int#>)"
// CHECK-NORMAL-NEXT:     },
// CHECK-NORMAL-NEXT:     {
// CHECK-NORMAL-NEXT:       key.kind: source.lang.swift.decl.var.instance,
// CHECK-NORMAL-NEXT:       key.name: "fooInstanceVar",
// CHECK-NORMAL-NEXT:       key.doc.full_as_xml: "<Other file=\"{{.*}}\" line=\"{{.*}}\" column=\"{{.*}}\"><Name>fooInstanceVar</Name><USR>s:15complete_member11FooProtocolP14fooInstanceVarSivp</USR><Declaration>var fooInstanceVar: Int { get }</Declaration><CommentParts><Abstract><Para>fooInstanceVar Aaa. Bbb.</Para></Abstract><Discussion><Para>Ccc.</Para></Discussion></CommentParts></Other>",
// CHECK-NORMAL-NEXT:       key.description: "fooInstanceVar",
// CHECK-NORMAL-NEXT:       key.typename: "Int",
// CHECK-NORMAL-NEXT:       key.doc.brief: "fooInstanceVar Aaa. Bbb.",
// CHECK-NORMAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NORMAL-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NORMAL-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NORMAL-NEXT:       key.associated_usrs: "s:15complete_member11FooProtocolP14fooInstanceVarSivp",
// CHECK-NORMAL-NEXT:       key.modulename: "complete_member",
// CHECK-NORMAL-NEXT:       key.sourcetext: "fooInstanceVar"
// CHECK-NORMAL-NEXT:     },
// CHECK-NORMAL-NEXT:     {
// CHECK-NORMAL-NEXT:       key.kind: source.lang.swift.keyword,
// CHECK-NORMAL-NEXT:       key.name: "self",
// CHECK-NORMAL-NEXT:       key.description: "self",
// CHECK-NORMAL-NEXT:       key.typename: "any FooProtocol",
// CHECK-NORMAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NORMAL-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NORMAL-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NORMAL-NEXT:       key.sourcetext: "self"
// CHECK-NORMAL-NEXT:     }
// CHECK-NORMAL-NEXT:   ]
// CHECK-NORMAL-NEXT: }

// RUN: %sourcekitd-test -req=complete -pos=19:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-OPTIONAL
// RUN: %sourcekitd-test -req=complete.open -pos=19:5 %s -- %s | %FileCheck %s -check-prefix=CHECK-OPTIONAL-OPEN
// CHECK-OPTIONAL:     {
// CHECK-OPTIONAL:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-OPTIONAL:       key.name: "fooInstanceFunc0()",
// CHECK-OPTIONAL-LABEL:       key.description: "fooInstanceFunc1(a: Int)",
// CHECK-OPTIONAL-NEXT:       key.typename: "Double",
// CHECK-OPTIONAL-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-OPTIONAL-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-OPTIONAL-NEXT:       key.num_bytes_to_erase: 1,
// CHECK-OPTIONAL-NEXT:       key.associated_usrs: "s:15complete_member11FooProtocolP16fooInstanceFunc1ySdSiF",
// CHECK-OPTIONAL-NEXT:       key.modulename: "complete_member",
// CHECK-OPTIONAL-NEXT:       key.sourcetext: "?.fooInstanceFunc1(<#T##a: Int##Int#>)"
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
// CHECK-OVERRIDE_USR-NEXT:     key.description: "foo()",
// CHECK-OVERRIDE_USR-NEXT:     key.typename: "Void",
// CHECK-OVERRIDE_USR-NEXT:     key.context: source.codecompletion.context.thisclass,
// CHECK-OVERRIDE_USR-NEXT:     key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-OVERRIDE_USR-NEXT:     key.num_bytes_to_erase: 0,
// CHECK-OVERRIDE_USR-NEXT:     key.associated_usrs: "s:15complete_member7DerivedC3fooyyF s:15complete_member4BaseC3fooyyF",
// CHECK-OVERRIDE_USR-NEXT:     key.modulename: "complete_member",
// CHECK-OVERRIDE_USR-NEXT:     key.sourcetext: "foo()"
// CHECK-OVERRIDE_USR-NEXT: }
