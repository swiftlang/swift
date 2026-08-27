// REQUIRES: swift_swift_parser
// REQUIRES: concurrency

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the macro library
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -strict-concurrency=complete -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/main.swift

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

// Emits `var wrapped: SendableWrapper<C> { fatalError() }`.
public struct AddWrappedPropertyMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      "var wrapped: SendableWrapper<C> { fatalError() }"
    ]
  }
}

//--- main.swift
protocol P {
  func f()
}

@MainActor
class C: @MainActor P {
  func f() { }
}

struct SendableWrapper<T: P & Sendable> {}
// expected-note@-1 {{requirement specified as 'T' : 'P' [with T = C]}}

@attached(member, names: named(wrapped))
macro AddWrappedProperty() = #externalMacro(module: "MacroDefinition", type: "AddWrappedPropertyMacro")

// expected-note@+1 {{in expansion of macro 'AddWrappedProperty' on struct 'UsesWrapper' here}}
@AddWrappedProperty
struct UsesWrapper {}
/*
expected-expansion@-2:21{{
  expected-error@1:14{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}}
*/
