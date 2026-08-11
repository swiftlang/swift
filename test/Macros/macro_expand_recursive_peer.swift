// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: not %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %t/macro_expand_recursive_peer.swift -disable-availability-checking 2>&1 | %FileCheck %s

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

/// Peer macro that expands to a declaration annotated with itself, to check
/// that recursive expansion is diagnosed rather than expanded forever.
public struct RecursivePeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      @RecursivePeer
      func \(context.makeUniqueName("again"))() {}
      """
    ]
  }
}

//--- macro_expand_recursive_peer.swift

// A peer macro that expands to a declaration annotated with itself must be
// diagnosed rather than expanded forever. Name lookup walks into declarations
// introduced by macros nested inside other macro expansions, so it can reach
// the recursive expansion; the check that stops it lives in macro expansion
// itself.

@attached(peer)
macro RecursivePeer() = #externalMacro(module: "MacroDefinition", type: "RecursivePeerMacro")

// At file scope.
@RecursivePeer
func topLevelRecursive() {}

// In a type context, which uses a different name lookup path.
struct S {
  @RecursivePeer
  func inTypeRecursive() {}
}

// CHECK-DAG: error: recursive expansion of macro 'RecursivePeer()'
// CHECK-DAG: in expansion of macro 'RecursivePeer' on global function 'topLevelRecursive()' here
// CHECK-DAG: in expansion of macro 'RecursivePeer' on instance method 'inTypeRecursive()' here
