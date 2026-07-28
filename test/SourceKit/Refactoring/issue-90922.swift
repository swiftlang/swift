// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/plugin.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -dump-ast %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -o /dev/null

// REQUIRES: swift_swift_parser

// https://github.com/swiftlang/swift/issues/90922

//--- plugin.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct EmptyMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    []
  }
}

//--- main.swift
@attached(peer)
macro M(_ fn: (Int) -> Void) = #externalMacro(module: "MacroDefinition", type: "EmptyMacro")

// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=%(line+2):6 %t/main.swift -- %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) | %FileCheck %s
@M({ _ in })
func foo() {}
// CHECK: [[@LINE-1]]:6-[[@LINE-1]]:9 source.refactoring.range.kind.basename
