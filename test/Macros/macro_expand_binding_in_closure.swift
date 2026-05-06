// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

// FIXME: This ought to not crash (https://github.com/swiftlang/swift/issues/88740)
// RUN: not --crash %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -DCRASH

//--- macro.swift

import SwiftSyntax
import SwiftSyntaxMacros

struct BindingMacro: DeclarationMacro, PeerMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["let \(context.makeUniqueName("x")) = 0"]
  }

  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["let \(context.makeUniqueName("y")) = 0"]
  }
}

struct IfExprBindingMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["let \(context.makeUniqueName("z")) = if .random() { return 0 } else { 0 }"]
  }
}

//--- main.swift

@freestanding(declaration)
macro NewBinding() = #externalMacro(module: "MacroDefinition", type: "BindingMacro")

@attached(peer)
macro PeerBinding() = #externalMacro(module: "MacroDefinition", type: "BindingMacro")

@freestanding(declaration)
macro IfExprBinding() = #externalMacro(module: "MacroDefinition", type: "IfExprBindingMacro")

func foo() {
  // Make sure we can type-check the bindings in the closures here.
  _ = {
    #NewBinding // expected-note {{in expansion of macro}}
    // expected-expansion@-1:5 {{
    //   expected-warning@1 {{initialization of immutable value}}
    // }}
    @PeerBinding
    func bar() {}
  }

  #if CRASH
  _ = {
    #IfExprBinding
  }
  #endif
}

