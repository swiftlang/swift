// REQUIRES: swift_swift_parser

// This test ensures that a unique name introduced by a macro expansion is only
// visible inside that macro expansion, so neither code outside of macro
// expansions nor other macro expansions can refer to it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UniqueNameMacros) -module-name=UniqueNameMacros %t/UniqueNameMacros.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -module-name M -load-plugin-library %t/%target-library-name(UniqueNameMacros) -primary-file %t/b.swift %t/a.swift %t/macros.swift

// Check again with the declaring macro expanded before the references.
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -module-name M -load-plugin-library %t/%target-library-name(UniqueNameMacros) %t/a.swift %t/b.swift %t/macros.swift

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

/// Adds a uniquely named peer function that calls the function named by the
/// string literal argument.
public struct CallingPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let callee = node.arguments!.as(LabeledExprListSyntax.self)!.first!
      .expression.as(StringLiteralExprSyntax.self)!.representedLiteralValue!
    return ["func \(context.makeUniqueName("caller"))() { \(raw: callee)() }"]
  }
}

//--- macros.swift

@attached(peer)
macro UniquePeer() = #externalMacro(module: "UniqueNameMacros", type: "UniquePeerMacro")

@attached(peer)
macro CallingPeer(_ callee: String) = #externalMacro(module: "UniqueNameMacros", type: "CallingPeerMacro")

//--- a.swift

@UniquePeer
func f() {}

//--- b.swift

@CallingPeer("$s1M1f10UniquePeerfMp_4peerfMu_") // expected-note {{in expansion of macro 'CallingPeer' on global function 'g()' here}}
func g() {}
// expected-expansion@-1:12 {{
//   expected-error@2 {{cannot find '$s1M1f10UniquePeerfMp_4peerfMu_' in scope}}
// }}

func h() {
  $s1M1f10UniquePeerfMp_4peerfMu_() // expected-error {{cannot find '$s1M1f10UniquePeerfMp_4peerfMu_' in scope}}
}
