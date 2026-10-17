// REQUIRES: swift_swift_parser

// This test ensures that looking up a unique name introduced by a top-level
// macro expansion only expands that macro, not the top-level macros in other
// files. The macros below declare a uniquely named declaration and refer to it
// from another uniquely named declaration, so both the redeclaration check and
// the reference look the unique name up at module scope.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UniqueNameMacros) -module-name=UniqueNameMacros %t/UniqueNameMacros.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(UniqueNameMacros) -stats-output-dir %t/stats -fine-grained-timers -print-zero-stats -primary-file %t/a.swift %t/b.swift %t/c.swift %t/macros.swift

// Only the primary file's two macros should be resolved, and only its
// freestanding macro should be expanded.
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'ResolveMacroRequest == 2' %t/stats
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'ExpandMacroExpansionDeclRequest == 1' %t/stats

//--- UniqueNameMacros.swift

import SwiftSyntax
import SwiftSyntaxMacros

/// Declares a uniquely named struct and a uniquely named variable that refers
/// to it.
public struct SelfReferencingDeclarationMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let type = context.makeUniqueName("Type")
    let value = context.makeUniqueName("value")
    return [
      "struct \(type) { static func make() -> Int { 1 } }",
      "let \(value) = \(type).make()",
    ]
  }
}

/// Adds a uniquely named peer function and a uniquely named peer variable that
/// refers to it.
public struct SelfReferencingPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let function = context.makeUniqueName("function")
    let value = context.makeUniqueName("value")
    return [
      "func \(function)() -> Int { 1 }",
      "let \(value) = \(function)()",
    ]
  }
}

//--- macros.swift

@freestanding(declaration)
macro selfReferencing() = #externalMacro(module: "UniqueNameMacros", type: "SelfReferencingDeclarationMacro")

@attached(peer)
macro SelfReferencingPeer() = #externalMacro(module: "UniqueNameMacros", type: "SelfReferencingPeerMacro")

//--- a.swift

#selfReferencing

@SelfReferencingPeer
struct A {}

//--- b.swift

#selfReferencing

@SelfReferencingPeer
struct B {}

//--- c.swift

#selfReferencing

@SelfReferencingPeer
struct C {}
