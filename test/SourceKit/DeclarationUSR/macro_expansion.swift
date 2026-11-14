// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath
// The expansions must succeed, otherwise the checks below would pass trivially.
// RUN: %target-swift-frontend -typecheck %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser
// RUN: %sourcekitd-test -req=collect-decl-usr %t/main.swift -- %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser | %FileCheck %s

// Declarations produced by macro expansions are not written in the source
// file, so they must not be reported.

// CHECK-NOT: generated
// CHECK:      key.kind: source.lang.swift.decl.macro
// CHECK-NEXT: key.usr: "s:9MacroUser7AddPeeryycfm"
// CHECK-NOT: generated
// CHECK:      key.kind: source.lang.swift.decl.macro
// CHECK-NEXT: key.usr: "s:9MacroUser7AddFuncyycfm"
// CHECK-NOT: generated
// CHECK:      key.kind: source.lang.swift.decl.function.free
// CHECK-NEXT: key.usr: "s:9MacroUser7writtenyyF"
// CHECK-NOT: generated
// CHECK:      key.kind: source.lang.swift.decl.function.free
// CHECK-NEXT: key.usr: "s:9MacroUser4useryyF"
// CHECK-NOT: generated

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

public struct AddFuncMacro: DeclarationMacro, PeerMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> [DeclSyntax] {
    ["func generatedFreestanding() {}"]
  }
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["func generatedPeer() {}"]
  }
}

//--- main.swift

@attached(peer, names: named(generatedPeer))
macro AddPeer() = #externalMacro(module: "MacroDefinition", type: "AddFuncMacro")

@freestanding(declaration, names: named(generatedFreestanding))
macro AddFunc() = #externalMacro(module: "MacroDefinition", type: "AddFuncMacro")

@AddPeer
func written() {}

#AddFunc

func user() {
  generatedPeer()
  generatedFreestanding()
}
