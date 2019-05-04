// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/fixit_stub_mutability_proto_module.swift -emit-module -parse-as-library -o %t

// RUN: %target-swift-frontend -typecheck %s -I %t -verify -diagnostics-editor-mode

protocol P1 {
  @available(*, deprecated)
  func foo1()
  func foo2()
}

protocol P2 {
  func bar1()
  func bar2()
}

class C1 : P1, P2 {} // expected-error{{type 'C1' does not conform to protocol 'P1'}} expected-error{{type 'C1' does not conform to protocol 'P2'}} expected-note{{do you want to add protocol stubs?}}{{20-20=\n    func foo1() {\n        <#code#>\n    \}\n\n    func foo2() {\n        <#code#>\n    \}\n\n    func bar1() {\n        <#code#>\n    \}\n\n    func bar2() {\n        <#code#>\n    \}\n}}

protocol P3 {
  associatedtype T1
  associatedtype T2
  associatedtype T3
}

protocol P4 : P3 {
  associatedtype T4 = T1
  associatedtype T5 = T2
  associatedtype T6 = T3
}

class C2 : P4 {} // expected-error{{type 'C2' does not conform to protocol 'P4'}} expected-error{{type 'C2' does not conform to protocol 'P3'}} expected-note{{do you want to add protocol stubs?}}{{16-16=\n    typealias T1 = <#type#>\n\n    typealias T2 = <#type#>\n\n    typealias T3 = <#type#>\n}}


// =============================================================================
// Test how we print stubs for mutating and non-mutating requirements.
//
// - Test that we don't print 'mutating' in classes.
// - Test that we print 'non-mutating' for non-mutating setters
//   in structs.
// =============================================================================

protocol LocalMutabilityProto {
    mutating func foo()
    subscript() -> Int { get nonmutating set }
}

class Class1: LocalMutabilityProto { // expected-error{{type 'Class1' does not conform to protocol 'LocalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{37-37=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct1: LocalMutabilityProto { // expected-error{{type 'Struct1' does not conform to protocol 'LocalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{39-39=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        nonmutating set {\n            <#code#>\n        \}\n    \}\n}}
}

import fixit_stub_mutability_proto_module

class Class2: ExternalMutabilityProto { // expected-error{{type 'Class2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{40-40=\n    func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        get {\n            <#code#>\n        \}\n        set(newValue) {\n            <#code#>\n        \}\n    \}\n}}
}

struct Struct2: ExternalMutabilityProto { // expected-error{{type 'Struct2' does not conform to protocol 'ExternalMutabilityProto'}} expected-note{{do you want to add protocol stubs?}} {{42-42=\n    mutating func foo() {\n        <#code#>\n    \}\n\n    subscript() -> Int {\n        mutating get {\n            <#code#>\n        \}\n        nonmutating set(newValue) {\n            <#code#>\n        \}\n    \}\n}}
}
