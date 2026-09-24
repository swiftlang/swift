// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %t/Test.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library | %FileCheck %s

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct EmptyPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return []
  }
}

//--- Test.swift
@attached(peer)
macro test(_ first: (Int) -> Int?, _ second: (Int) -> Int?) =
  #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")

// Matching parameter names within and across custom attributes should have
// distinct USRs.
@test({ value in nil }, { value in nil })
func first() {}

// CHECK: [[@LINE-3]]:9 | param(local)/Swift | value | s:14swift_ide_testSiSgSiXEfU_5valueL_Sivp | Def | rel: 0
// CHECK: [[@LINE-4]]:27 | param(local)/Swift | value | s:14swift_ide_testSiSgSiXEfU0_5valueL_Sivp | Def | rel: 0

@test({ value in nil }, { value in nil })
func second() {}

// CHECK: [[@LINE-3]]:9 | param(local)/Swift | value | s:14swift_ide_testSiSgSiXEfU1_5valueL_Sivp | Def | rel: 0
// CHECK: [[@LINE-4]]:27 | param(local)/Swift | value | s:14swift_ide_testSiSgSiXEfU2_5valueL_Sivp | Def | rel: 0
