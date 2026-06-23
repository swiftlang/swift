// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift
// RUN: not --crash %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) -DCRASH %t/main.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct UseBeforeDeclRef: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    "print(x)"
  }
}

public struct UseBeforeDecl: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    [
      """
      print(y)
      let y = 0
      """
    ]
  }
}

//--- main.swift
@freestanding(expression)
public macro UseBeforeDeclRef() = #externalMacro(module: "MacroDefinition", type: "UseBeforeDeclRef")

#UseBeforeDeclRef // expected-note {{in expansion of macro 'UseBeforeDeclRef' here}}
// expected-expansion@-1:1{{
//   expected-error@1 {{use of global variable 'x' before its declaration}}
// }}
let x = 0 // expected-note {{'x' declared here}}
#UseBeforeDeclRef

@freestanding(declaration)
public macro UseBeforeDecl() = #externalMacro(module: "MacroDefinition", type: "UseBeforeDecl")

// FIXME: This currently crashes the compiler, once fixed insert the right
// diagnostic expectation.
#if CRASH
#UseBeforeDecl()
#endif
