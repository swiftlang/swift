// {"kind":"typecheck","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Verifier::verifyChecked"}
// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath
// RUN: not --crash %target-swift-frontend -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift
//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct EmptyDeclarationMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return []
  }
}

public struct EmptyPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return []
  }
}
//--- main.swift
@freestanding(declaration) macro empty() = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")
@attached(peer) macro Peer() = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
func f(_ a: Int?, _ b: Int) -> Int {
  func h() {
    @Peer #empty
  }
  return a ?? b
}
