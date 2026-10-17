// REQUIRES: swift_swift_parser

// This test ensures that the redeclaration check catches colliding unique
// names introduced by peer macro expansions in different files.
//
// The unique name of a peer macro expansion encodes the attached-to
// declaration's base name but not its full signature, so expanding the same
// peer macro on overloads of 'f' in different files produces the same unique
// name. If the mangling starts distinguishing overloads, the names no longer
// collide and this test needs to pick colliding expansions another way.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UniqueNameMacros) -module-name=UniqueNameMacros %t/UniqueNameMacros.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -module-name M -load-plugin-library %t/%target-library-name(UniqueNameMacros) -primary-file %t/a.swift %t/b.swift %t/macros.swift

//--- UniqueNameMacros.swift

import SwiftSyntax
import SwiftSyntaxMacros

/// Adds a uniquely named peer function.
public struct UniquePeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["func \(context.makeUniqueName("peer"))() {}"]
  }
}

//--- macros.swift

@attached(peer)
macro UniquePeer() = #externalMacro(module: "UniqueNameMacros", type: "UniquePeerMacro")

//--- a.swift

@UniquePeer // expected-note {{in expansion of macro 'UniquePeer' on global function 'f(x:)' here}}
func f(x: Int) {}
// expected-expansion@-1:18 {{
//   expected-error@1 {{invalid redeclaration of '$s1M1f10UniquePeerfMp_4peerfMu_()'}}
// }}

//--- b.swift

@UniquePeer
func f(y: Int) {}
// expected-expansion@-1:18 {{
//   expected-note@1 {{'$s1M1f10UniquePeerfMp_4peerfMu_()' previously declared here}}
// }}
