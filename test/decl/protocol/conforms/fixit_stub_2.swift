// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/fixit_stub_mutability_proto_module.swift -emit-module -parse-as-library -o %t

// RUN: %target-swift-frontend -typecheck %s -I %t -verify

protocol P0_A { associatedtype T } // expected-note{{protocol requires nested type 'T'}}
protocol P0_B { associatedtype T }

class C0: P0_A, P0_B {} // expected-error{{type 'C0' does not conform to protocol 'P0_A'}} expected-error{{type 'C0' does not conform to protocol 'P0_B'}} expected-note{{add stubs for conformance}}{{23-23=\n    typealias T = <#type#>\n}}

protocol P1 {
  @available(*, deprecated)
  func foo1() // expected-note{{protocol requires function 'foo1()' with type '() -> ()'}}
  func foo2() // expected-note{{protocol requires function 'foo2()' with type '() -> ()'}}
  func foo3(arg: Int, arg2: String) // expected-note{{protocol requires function 'foo3(arg:arg2:)' with type '(Int, String) -> ()'}}
  func foo4<T: P1>(_: T) // expected-note{{protocol requires function 'foo4' with type '<T> (T) -> ()'}}
}

protocol P2 {
  func bar1() // expected-note{{protocol requires function 'bar1()' with type '() -> ()'}}
  func bar2() // expected-note{{protocol requires function 'bar2()' with type '() -> ()'}}

  func foo2()
  func foo3(arg: Int, arg2: String)
  func foo3(arg: Int, arg2: Int)  // expected-note{{protocol requires function 'foo3(arg:arg2:)' with type '(Int, Int) -> ()'}}
  func foo4<T: P1>(_: T)
  func foo4<T: P2>(_: T)
}

class C1 : P1, P2 {} // expected-error{{type 'C1' does not conform to protocol 'P1'}} expected-error{{type 'C1' does not conform to protocol 'P2'}} expected-note{{add stubs for conformance}}{{20-20=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func foo3(arg: Int, arg2: String) {\n        <#code#>\n    \}\n\n    func foo4<T>(_: T) where T : P1 {\n        <#code#>\n    \}\n\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n\n    func foo3(arg: Int, arg2: Int) {\n        <#code#>\n    \}\n}}

protocol P3 {
  associatedtype T1
  associatedtype T2 // expected-note{{protocol requires nested type 'T2'}}
  associatedtype T3 // expected-note{{protocol requires nested type 'T3'}}
}

protocol P4 : P3 {
  associatedtype T1 // expected-note{{protocol requires nested type 'T1'}}
  associatedtype T4 = T1 // expected-note{{protocol requires nested type 'T4'}}
  associatedtype T5 = T2 // expected-note{{protocol requires nested type 'T5'}}
  associatedtype T6 = T3 // expected-note{{protocol requires nested type 'T6'}}
}

class C2 : P4 {} // expected-error{{type 'C2' does not conform to protocol 'P4'}} expected-error{{type 'C2' does not conform to protocol 'P3'}} expected-note{{add stubs for conformance}}{{16-16=\n    typealias T1 = <#type#>\n\n    typealias T2 = <#type#>\n\n    typealias T3 = <#type#>\n}}

protocol P5 {
  func foo1()
  func foo2(arg: Int, arg2: String)
  func foo3<T: P3>(_: T)
}

protocol P6: P5 {
  func foo1() // expected-note{{protocol requires function 'foo1()' with type '() -> ()'}}
  func foo2(arg: Int, arg2: String) // expected-note{{protocol requires function 'foo2(arg:arg2:)' with type '(Int, String) -> ()'}}
  func foo2(arg: Int, arg2: Int) // expected-note{{protocol requires function 'foo2(arg:arg2:)' with type '(Int, Int) -> ()'}}
  func foo3<T: P3>(_: T) // expected-note{{protocol requires function 'foo3' with type '<T> (T) -> ()'}}
  func foo3<T: P4>(_: T)
}

class C3 : P6 {} // expected-error{{type 'C3' does not conform to protocol 'P5'}} expected-error{{type 'C3' does not conform to protocol 'P6'}} expected-note{{add stubs for conformance}}{{16-16=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2(arg: Int, arg2: String) {\n        <#code#>\n    \}\n\n    func foo2(arg: Int, arg2: Int) {\n        <#code#>\n    \}\n\n    func foo3<T>(_: T) where T : P3 {\n        <#code#>\n    \}\n}}

// =============================================================================
// Test how we print stubs for mutating and non-mutating requirements.
//
// - Test that we don't print 'mutating' in classes.
// - Test that we print 'non-mutating' for non-mutating setters
//   in structs.
// =============================================================================

protocol MutabilityProto {
  mutating func foo() // expected-note2 {{protocol requires function 'foo()' with type '() -> ()'}}
  subscript() -> Int { get nonmutating set } // expected-note2{{protocol requires subscript with type '() -> Int'}}
}

class Class1: MutabilityProto { // expected-error{{type 'Class1' does not conform to protocol 'MutabilityProto'}} expected-note{{add stubs for conformance}} {{32-32=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct1: MutabilityProto { // expected-error{{type 'Struct1' does not conform to protocol 'MutabilityProto'}} expected-note{{add stubs for conformance}} {{34-34=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n}}
}

import fixit_stub_mutability_proto_module

class Class2: ExternalMutabilityProto { // expected-error{{type 'Class2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{add stubs for conformance}} {{40-40=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct2: ExternalMutabilityProto { // expected-error{{type 'Struct2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{add stubs for conformance}} {{42-42=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        mutating get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n}}
}

protocol PropertyMutabilityProto {
  var computed: Int { mutating get nonmutating set }  // expected-note3 {{protocol requires property 'computed' with type 'Int'}}
  var stored: Int { mutating get set } // expected-note3 {{protocol requires property 'stored' with type 'Int'}}
}

class Class3: PropertyMutabilityProto { // expected-error{{type 'Class3' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{add stubs for conformance}} {{40-40=\n    var computed: Int\n\n    var stored: Int\n}}
}

struct Struct3: PropertyMutabilityProto { // expected-error{{type 'Struct3' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{add stubs for conformance}} {{42-42=\n    var computed: Int {\n        mutating get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n\n    var stored: Int\n}}
}

class Class4 {}
extension Class4: PropertyMutabilityProto { // expected-error{{type 'Class4' does not conform to protocol 'PropertyMutabilityProto'}} expected-note{{add stubs for conformance}} {{44-44=\n    var computed: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n\n    var stored: Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

// https://github.com/apple/swift/issues/52274

protocol FooProto {
 typealias CompletionType = (Int) -> Void
 func doSomething(then completion: @escaping CompletionType) // expected-note{{protocol requires function 'doSomething(then:)' with type '(@escaping FooType.CompletionType) -> ()' (aka '(@escaping (Int) -> ()) -> ()')}}
}

struct FooType : FooProto { // expected-error {{type 'FooType' does not conform to protocol 'FooProto'}} expected-note {{add stubs for conformance}} {{28-28=\n    func doSomething(then completion: @escaping CompletionType) {\n        <#code#>\n    \}\n}}
}
