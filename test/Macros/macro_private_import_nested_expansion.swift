// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Build macro plugin
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// Build library module
// RUN: %target-swift-frontend -emit-module -module-name MyLib -o %t/MyLib.swiftmodule %t/MyLib.swift -enable-library-evolution

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroPlugin) -I %t %t/Client.swift -module-name Client

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxMacros
import Foundation

public struct StoredMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let varDecl = declaration.as(VariableDeclSyntax.self),
          let binding = varDecl.bindings.first,
          let pattern = binding.pattern.as(IdentifierPatternSyntax.self),
          let initializer = binding.initializer?.value
    else { return [] }

    let name = pattern.identifier.text
    let expr = initializer.description
    let encoded = Data(expr.utf8).base64EncodedString()

    return [
      """
      @_InitAccessor(\(literal: encoded))
      private static var __\(raw: name)_init = \(initializer)
      """
    ]
  }
}

public struct InitAccessorMacro: AccessorMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingAccessorsOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AccessorDeclSyntax] {
    guard let args = node.arguments?.as(LabeledExprListSyntax.self),
          let firstArg = args.first,
          let lit = firstArg.expression.as(StringLiteralExprSyntax.self),
          let seg = lit.segments.first?.as(StringSegmentSyntax.self)
    else { return [] }

    let encoded = seg.content.text
    guard let data = Data(base64Encoded: encoded),
          let decoded = String(data: data, encoding: .utf8)
    else { return [] }

    return [
      """
      get {
          \(raw: decoded)
      }
      """
    ]
  }
}

//--- MyLib.swift
public enum Direction {
  case north, south, east, west
}

//--- Client.swift
@attached(peer, names: arbitrary)
macro Stored() = #externalMacro(module: "MacroPlugin", type: "StoredMacro")

@attached(accessor, names: named(get))
macro _InitAccessor(_ expr: String) = #externalMacro(module: "MacroPlugin", type: "InitAccessorMacro")

private import MyLib

// Make sure we can lookup Direction.east from within the nested expansion.
struct ContentView {
  @Stored private var dir = Direction.east
}
