// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/fixit_stub_mutability_proto_module.swift -emit-module -parse-as-library -o %t

// RUN: %target-swift-frontend -typecheck %s -I %t -verify -diagnostics-editor-mode

protocol P0_A { associatedtype T }
protocol P0_B { associatedtype T }

class C0: P0_A, P0_B {} // expected-error{{type 'C0' does not conform to protocol 'P0_A'}} expected-error{{type 'C0' does not conform to protocol 'P0_B'}} expected-note{{do you want to add protocol stubs?}}{{23-23=\n    typealias T = <#type#>\n}}

protocol P1 {
  @available(*, deprecated)
  func foo1()
  func foo2()
  func foo3(arg: Int, arg2: String)
  func foo4<T: P1>(_: T)
}

protocol P2 {
  func bar1()
  func bar2()

  func foo2()
  func foo3(arg: Int, arg2: String)
  func foo3(arg: Int, arg2: Int)
  func foo4<T: P1>(_: T)
  func foo4<T: P2>(_: T)
}

class C1 : P1, P2 {} // expected-error{{type 'C1' does not conform to protocol 'P1'}} expected-error{{type 'C1' does not conform to protocol 'P2'}} expected-note{{do you want to add protocol stubs?}}{{20-20=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3(arg: Int, arg2: String) {\n        <#code#>\n    \}\n\n    func foo4<T>(_: T) where T : P1 {\n        <#code#>\n    \}\n\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func foo3(arg: Int, arg2: Int) {\n        <#code#>\n    \}\n}}

protocol P3 {
  associatedtype T1
  associatedtype T2
  associatedtype T3
}

protocol P4 : P3 {
  associatedtype T1
  associatedtype T4 = T1
  associatedtype T5 = T2
  associatedtype T6 = T3
}

class C2 : P4 {} // expected-error{{type 'C2' does not conform to protocol 'P4'}} expected-error{{type 'C2' does not conform to protocol 'P3'}} expected-note{{do you want to add protocol stubs?}}{{16-16=\n    typealias T1 = <#type#>\n\n    typealias T2 = <#type#>\n\n    typealias T3 = <#type#>\n}}

protocol P5 {
  func foo1()
  func foo2(arg: Int, arg2: String)
  func foo3<T: P3>(_: T)
}

protocol P6: P5 {
  func foo1()
  func foo2(arg: Int, arg2: String)
  func foo2(arg: Int, arg2: Int)
  func foo3<T: P3>(_: T)
  func foo3<T: P4>(_: T)
}

class C3 : P6 {} // expected-error{{type 'C3' does not conform to protocol 'P5'}} expected-error{{type 'C3' does not conform to protocol 'P6'}} expected-note{{do you want to add protocol stubs?}}{{16-16=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2(arg: Int, arg2: String) {\n        <#code#>\n    \}\n\n    func foo2(arg: Int, arg2: Int) {\n        <#code#>\n    \}\n\n    func foo3<T>(_: T) where T : P3 {\n        <#code#>\n    \}\n}}

// =============================================================================
// Test how we print stubs for mutating and non-mutating requirements.
//
// - Test that we don't print 'mutating' in classes.
// - Test that we print 'non-mutating' for non-mutating setters
//   in structs.
// =============================================================================

protocol MutabilityProto {
  mutating func foo()
  subscript() -> Int { get nonmutating set }
}

class Class1: MutabilityProto { // expected-error{{type 'Class1' does not conform to protocol 'MutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{32-32=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct1: MutabilityProto { // expected-error{{type 'Struct1' does not conform to protocol 'MutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{34-34=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n}}
}

import fixit_stub_mutability_proto_module

class Class2: ExternalMutabilityProto { // expected-error{{type 'Class2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{40-40=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set(newValue) {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct2: ExternalMutabilityProto { // expected-error{{type 'Struct2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{42-42=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        mutating get {\n            <#code#>\n        \}\n        nonmutating set(newValue) {\n            <#code#>\n        \}\n    \}\n}}
}

protocol PropertyMutabilityProto {
  var computed: Int { mutating get nonmutating set }
  var stored: Int { mutating get set }
}

class Class3: PropertyMutabilityProto { // expected-error{{type 'Class3' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{40-40=\n    var computed: Int\n\n    var stored: Int\n}}
}

struct Struct3: PropertyMutabilityProto { // expected-error{{type 'Struct3' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{42-42=\n    var computed: Int {\n        mutating get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n\n    var stored: Int\n}}
}

class Class4 {}
extension Class4: PropertyMutabilityProto { // expected-error{{type 'Class4' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{44-44=\n    var computed: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    var stored: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

// https://bugs.swift.org/browse/SR-9868
protocol FooProto {
 typealias CompletionType = (Int) -> Void
 func doSomething(then completion: @escaping CompletionType)
}

struct FooType : FooProto { // expected-error {{type 'FooType' does not conform to protocol 'FooProto'}} expected-note {{do you want to add protocol stubs?}} {{28-28=\n    func doSomething(then completion: @escaping CompletionType) {\n        <#code#>\n    \}\n}}
}
