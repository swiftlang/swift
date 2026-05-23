// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: not --crash %target-swift-frontend -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

// https://github.com/swiftlang/swift/issues/88444

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

struct DummyDeclMacro: DeclarationMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    [ "let foo = \(node.trailingClosure!)" ]
  }
}

//--- main.swift

@freestanding(declaration, names: named(foo))
macro DummyDeclMacro(_ fn: () -> Void) = #externalMacro(module: "MacroDefinition", type: "DummyDeclMacro")

let x = #DummyDeclMacro {}
