// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macros.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition)

//--- macros.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct AttrMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    []
  }
}

public struct MemberAttrMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    ["@AttrMacro"]
  }
}

//--- main.swift

@attached(peer, names: arbitrary)
macro AttrMacro() = #externalMacro(module: "MacroDefinition", type: "AttrMacro")

@attached(memberAttribute)
macro MemberAttrMacro(_ x: Int?) = #externalMacro(module: "MacroDefinition", type: "MemberAttrMacro")

@MemberAttrMacro(0)
struct S {
  var x: Int
}
