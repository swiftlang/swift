// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_DefaultIsolationPerFile

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the macro library
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift

// Type check and verify
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) %t/defaulted.swift %t/caller.swift

// This test verifies two properties of `using @available(...)` file
// defaults against peer macros:
//
// 1. Peer macro top-level decls DO inherit the file default.
// 2. Macros cannot emit `using` decls themselves.

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

/// Synthesizes a sibling function named "<original>_peer".
public struct AddPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let named = declaration.asProtocol(NamedDeclSyntax.self) else {
      return []
    }
    return [
      """
      public func \(raw: named.name.text)_peer() {}
      """
    ]
  }
}

/// Synthesizes a `using` decl as a peer (must reject).
public struct EmitUsingMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      using @available(*, deprecated, message: "from macro")
      """
    ]
  }
}

//--- defaulted.swift
using @available(*, deprecated, message: "legacy")

@attached(peer, names: suffixed(_peer))
macro AddPeer() = #externalMacro(module: "MacroDefinition", type: "AddPeerMacro")

@attached(peer)
macro EmitUsing() = #externalMacro(module: "MacroDefinition", type: "EmitUsingMacro")

@AddPeer
public func foo() {}

// expected-error@@__swiftmacro_9defaulted3bar9EmitUsingfMp_.swift:1:1 {{macro expansion cannot introduce using}}
@EmitUsing // expected-note {{in expansion of macro 'EmitUsing' on global function 'bar()' here}}
public func bar() {}

//--- caller.swift
public func uses() {
  foo() // expected-warning {{'foo()' is deprecated: legacy}}
  foo_peer() // expected-warning {{'foo_peer()' is deprecated: legacy}}
  bar() // expected-warning {{'bar()' is deprecated: legacy}}
}
