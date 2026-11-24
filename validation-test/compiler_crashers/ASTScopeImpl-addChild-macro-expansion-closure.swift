// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: not --crash %target-swift-frontend -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct IdentityMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    "\(node.arguments)"
  }
}

//--- main.swift
@freestanding(expression)
macro id<T>(_ x: T) -> T = #externalMacro(module: "MacroDefinition", type: "IdentityMacro")

struct S {
  var fn = #id({1})
}
