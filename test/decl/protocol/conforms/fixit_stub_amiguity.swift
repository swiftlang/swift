// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/fixit_stub_ambiguity_module.swift -module-name Ambiguous -emit-module -parse-as-library -o %t

// RUN: %target-swift-frontend -typecheck %s -I %t -verify

import Ambiguous

struct Notification {}

struct MyApp: AmbiguousFuncProtocol {
// expected-error@-1 {{type 'MyApp' does not conform to protocol 'AmbiguousFuncProtocol'}}
// expected-note@-2 {{add stubs for conformance}} {{38-38=\n    func application(received: Ambiguous.Notification) {\n        <#code#>\n    \}\n}}
}

extension MyApp: AmbiguousVarProtocol {
// expected-error@-1 {{type 'MyApp' does not conform to protocol 'AmbiguousVarProtocol'}}
// expected-note@-2 {{add stubs for conformance}} {{40-40=\n    var notification: Ambiguous.Notification? {\n        <#code#>\n    \}\n}}
}

// FIXME: There's a remaining common ambiguity that occurs with nested types
// in the same module:
/*

struct Outer { struct Inner {} }
struct Inner {}
protocol P { func foo(_ : Inner) }

extension Outer: P {} // fixit `func foo(_ : Inner)` picks up `Outer.Inner` by accident

The ASTPrinter doesn't currently have enough context to declare this situation ambiguous.

*/
