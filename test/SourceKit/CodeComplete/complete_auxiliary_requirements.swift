class Class {}
protocol Proto {
  associatedtype Assoc: Proto & Class
  associatedtype Bssoc: Proto
}
extension Proto {
  func genericMethod<U: Class>(_: Self, _: Assoc, _: U) {}

  func testProtoMembers() {
    self. // MEMBERS_PROTO_EXT
  }
}
struct GenericStruct<T: Proto> {
  func genericMethod1<U: Proto>(u1: U, u2: U, u3: U) {}
  func genericMethod2<U: Proto>(_: U.Bssoc, _: U, _: U.Assoc, _: U, _: T) {}

  subscript<U: Proto>(_: U) -> T { fatalError() }

  func testCallArgPatterns() {
    self.genericMethod1( // CALLARG_PATTERNS
  }
  func testSingleCallArgPattern<U: Proto>(_: Int, u: U) {
    self.testSingleCallArgPattern(0, // CALLARG_SINGLE
  }
}
func testStructMembers<X>(value: GenericStruct<X>) {
  value // MEMBERS_STRUCT
}

// Test that we place, arrange and annotate auxiliary requirements correctly.

// RUN: %sourcekitd-test -req=complete -pos=10:10 %s -- %s | %FileCheck %s -check-prefix=MEMBERS_PROTO_EXT
// RUN: %sourcekitd-test -req=complete -pos=20:25 %s -- %s | %FileCheck %s -check-prefix=CALLARG_PATTERNS
// RUN: %sourcekitd-test -req=complete -pos=23:37 %s -- %s | %FileCheck %s -check-prefix=CALLARG_SINGLE
// RUN: %sourcekitd-test -req=complete -pos=27:8 %s -- %s | %FileCheck %s -check-prefix=MEMBERS_STRUCT
// RUN: %sourcekitd-test -req=complete -pos=27:8 -req-opts=annotateddescription=1 %s -- %s | %FileCheck %s -check-prefix=MEMBERS_STRUCT_ANNOTATED

// MEMBERS_PROTO_EXT:      key.kind: source.lang.swift.decl.function.method.instance,
// MEMBERS_PROTO_EXT-NEXT: key.name: "genericMethod()",
// MEMBERS_PROTO_EXT-NEXT: key.sourcetext: "genericMethod(<#T##Self#>, <#T##Self.Assoc#>, <#T##U#>)",
// MEMBERS_PROTO_EXT-NEXT: key.description: "genericMethod(Self, Self.Assoc, U)",
// MEMBERS_PROTO_EXT-NEXT: key.typename: "Void where U : Class",

// FIXME: Call argument patterns need auxiliary requirements.
// CALLARG_PATTERNS:      key.kind: source.lang.swift.decl.function.method.instance,
// CALLARG_PATTERNS-NEXT: key.name: "u1:u2:u3:",
// CALLARG_PATTERNS-NEXT: key.sourcetext: "u1: <#T##U#>, u2: <#T##U#>, u3: <#T##U#>",
// CALLARG_PATTERNS-NEXT: key.description: "(u1: U, u2: U, u3: U)",
// CALLARG_PATTERNS-NEXT: key.typename: "Void",

// FIXME: Individual call argument patterns also deserve auxiliary requirements.
// CALLARG_SINGLE:      key.kind: source.lang.swift.pattern,
// CALLARG_SINGLE-NEXT: key.name: "u:",
// CALLARG_SINGLE-NEXT: key.sourcetext: "u: <#T##U#>",
// CALLARG_SINGLE-NEXT: key.description: "u: U",
// CALLARG_SINGLE-NEXT: key.typename: "U",

// MEMBERS_STRUCT:      key.kind: source.lang.swift.decl.function.subscript,
// MEMBERS_STRUCT-NEXT: key.name: "[]",
// MEMBERS_STRUCT-NEXT: key.sourcetext: "[<#T##U#>]",
// MEMBERS_STRUCT-NEXT: key.description: "[U]",
// MEMBERS_STRUCT-NEXT: key.typename: "X where U : Proto",
// MEMBERS_STRUCT:      {
// MEMBERS_STRUCT-NEXT: key.kind: source.lang.swift.decl.function.method.instance,
// MEMBERS_STRUCT-NEXT: key.name: "genericMethod1(u1:u2:u3:)",
// MEMBERS_STRUCT-NEXT: key.sourcetext: ".genericMethod1(u1: <#T##U#>, u2: <#T##U#>, u3: <#T##U#>)",
// MEMBERS_STRUCT-NEXT: key.description: "genericMethod1(u1: U, u2: U, u3: U)",
// MEMBERS_STRUCT-NEXT: key.typename: "Void where U : Proto",
// MEMBERS_STRUCT:      {
// MEMBERS_STRUCT-NEXT: key.kind: source.lang.swift.decl.function.method.instance,
// MEMBERS_STRUCT-NEXT: key.name: "genericMethod2()",
// MEMBERS_STRUCT-NEXT: key.sourcetext: ".genericMethod2(<#T##U.Bssoc#>, <#T##U#>, <#T##U.Assoc#>, <#T##U#>, <#T##X#>)",
// MEMBERS_STRUCT-NEXT: key.description: "genericMethod2(U.Bssoc, U, U.Assoc, U, X)",
// MEMBERS_STRUCT-NEXT: key.typename: "Void where U : Proto, U.Assoc : Class & Proto, U.Bssoc : Proto",

// MEMBERS_STRUCT_ANNOTATED:      key.kind: source.lang.swift.decl.function.method.instance,
// MEMBERS_STRUCT_ANNOTATED:      key.name: "genericMethod2()",
// MEMBERS_STRUCT_ANNOTATED-NEXT: key.sourcetext: ".genericMethod2(<#T##U.Bssoc#>, <#T##U#>, <#T##U.Assoc#>, <#T##U#>, <#T##X#>)",
// MEMBERS_STRUCT_ANNOTATED-NEXT: key.description: "<name>genericMethod2</name>(<callarg><callarg.type><typeid.user>U</typeid.user>.<typeid.user>Bssoc</typeid.user></callarg.type></callarg>, <callarg><callarg.type><typeid.user>U</typeid.user></callarg.type></callarg>, <callarg><callarg.type><typeid.user>U</typeid.user>.<typeid.user>Assoc</typeid.user></callarg.type></callarg>, <callarg><callarg.type><typeid.user>U</typeid.user></callarg.type></callarg>, <callarg><callarg.type><typeid.user>X</typeid.user></callarg.type></callarg>)",
// MEMBERS_STRUCT_ANNOTATED-NEXT: key.typename: "<typeid.sys>Void</typeid.sys> <keyword>where</keyword> <typeid.user>U</typeid.user> : <typeid.user>Proto</typeid.user>, <typeid.user>U</typeid.user>.<typeid.user>Assoc</typeid.user> : <typeid.user>Class</typeid.user> &amp; <typeid.user>Proto</typeid.user>, <typeid.user>U</typeid.user>.<typeid.user>Bssoc</typeid.user> : <typeid.user>Proto</typeid.user>",
