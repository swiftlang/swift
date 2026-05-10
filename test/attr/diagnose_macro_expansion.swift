// REQUIRES: swift_swift_parser, executable_test
// REQUIRES: swift_feature_SourceWarningControl

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the macro library
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift

// Type check and verify the test itself which applies @diagnose to a macro expansion expression
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature SourceWarningControl -load-plugin-library %t/%target-library-name(MacroDefinition) %t/test.swift

// This test verifies that @diagnose effect applies to everything within a macro expansion, and that @warn attributes within the expanded
// code can override the expansion-statement control

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct DefineBarTestMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let results: [DeclSyntax] = [
      """
      class \(context.makeUniqueName("name")) {
        func hello() -> String {
          return bar()
          //return "Hello, World"
        }
      }
      """,
      """
      @diagnose(DeprecatedDeclaration, as: ignored)
      struct \(context.makeUniqueName("name")) {
        func hello() -> String {
          return bar()
          //return "Hello, World"
        }
      }
      """
    ]
    return results
  }
}

//--- test.swift

@available(*, deprecated)
func bar() -> String { return "GoodBye" }

@freestanding(declaration)
macro anonymousTypes() = #externalMacro(module: "MacroDefinition", type: "DefineBarTestMacro")

@diagnose(DeprecatedDeclaration, as: error)
#anonymousTypes() // expected-note {{in expansion of macro 'anonymousTypes' here}}

// expected-error@@__swiftmacro_4test0015testswift_tzEGbfMX8_0_33_83378C430F65473055F1BD53F3ADCDB7Ll14anonymousTypesfMf_.swift:3:12{{'bar()' is deprecated}}
