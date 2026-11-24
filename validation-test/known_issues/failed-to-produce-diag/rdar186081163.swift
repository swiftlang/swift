// rdar://186081163
// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct IdentityBody: BodyMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    guard let body = declaration.body else { return [] }
    return Array(body.statements)
  }
}

//--- main.swift
@attached(body)
macro IdentityBody() = #externalMacro(module: "MacroDefinition", type: "IdentityBody")

@resultBuilder
enum MyBuilder {
  static func buildBlock<T>(_ x: T) -> T { x }
}

let g = 7

struct S {
  @MyBuilder
  @IdentityBody
  var thing: Int { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    let _ = g
    42
  }
}
