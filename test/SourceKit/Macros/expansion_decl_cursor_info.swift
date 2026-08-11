// RUN: %empty-directory(%t)
// REQUIRES: swift_swift_parser

// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

public struct AddFuncMacro: DeclarationMacro, PeerMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> [DeclSyntax] {
    ["func foo(_ x: String) {}"]
  }
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["func foo(_ x: Int) {}"]
  }
}

//--- main.swift

@attached(peer, names: named(foo))
macro AddFuncPeer(x: Int) = #externalMacro(module: "MacroDefinition", type: "AddFuncMacro")

@freestanding(declaration, names: named(foo))
macro AddFunc(x: Int) = #externalMacro(module: "MacroDefinition", type: "AddFuncMacro")

// Note that the peer macro is deliberately not attached to the freestanding
// macro expansion below. Attributes written on a macro expansion declaration
// are inherited by the declarations it expands to, so attaching the peer macro
// to it would expand the peer macro a second time, against the expanded
// 'foo(_: String)', and the two expansions would both declare 'foo(_: Int)'.
@AddFuncPeer(x: 0)
func other() {}

#AddFunc(x: 0)

foo(0)
// RUN: %sourcekitd-test -req=cursor %t/main.swift -pos=%(line-1):1 -- %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser | %FileCheck --check-prefix PEER %s
// PEER: source.lang.swift.ref.function.free ([[@LINE-7]]:1-[[@LINE-7]]:19)

foo("")
// RUN: %sourcekitd-test -req=cursor %t/main.swift -pos=%(line-1):1 -- %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser | %FileCheck --check-prefix FREESTANDING %s
// FREESTANDING: source.lang.swift.ref.function.free ([[@LINE-8]]:1-[[@LINE-8]]:15)
