// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// Build macros
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/src/macro_definitions.swift -g -no-toolchain-stdlib-rpath

/// Build the library
// RUN: %target-swift-frontend -emit-module %t/src/Lib.swift \
// RUN:   -module-name Lib \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -module-name Lib

// Build the client using module
// RUN: %target-swift-frontend -typecheck -verify -language-mode 6 -load-plugin-library %t/%target-library-name(MacroDefinition) -default-isolation MainActor -module-name Client -I %t %t/src/Client.swift
// RUN: %target-swift-frontend -typecheck -verify -language-mode 6 -load-plugin-library %t/%target-library-name(MacroDefinition) -default-isolation MainActor -module-name ClientWithMacro -I %t %t/src/ClientWithMacro.swift

// RUN: rm %t/Lib.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-frontend -typecheck -verify -language-mode 6 -load-plugin-library %t/%target-library-name(MacroDefinition) -default-isolation MainActor -module-name Client -I %t %t/src/Client.swift
// RUN: %target-swift-frontend -typecheck -verify -language-mode 6 -load-plugin-library %t/%target-library-name(MacroDefinition) -default-isolation MainActor -module-name ClientWithMacro -I %t %t/src/ClientWithMacro.swift

// REQUIRES: swift_swift_parser, executable_test

//--- macro_definitions.swift
import SwiftOperators
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax
import SwiftSyntaxBuilder
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

public struct AddConformanceMacro: ExtensionMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    let conformance: DeclSyntax =
      """
      extension \(type.trimmed): Q {}
      """

    guard let extensionDecl = conformance.as(ExtensionDeclSyntax.self) else {
      return []
    }

    return [extensionDecl]
  }
}

//--- Lib.swift
public protocol Other {
}

public protocol P: Sendable {
}

public protocol Q: P, Other {
}

public protocol W: Q {
}

@attached(extension, conformances: Q)
public macro AddQ() = #externalMacro(module: "MacroDefinition", type: "AddConformanceMacro")

//--- ClientWithMacro.swift
import Lib

@AddQ
struct S: W {
  var x: Int // Ok (no errors about conformance isolation)
}

//--- Client.swift
import Lib

struct S: P {
}

extension S: Q { // Ok (no errors about conformance isolation)
}

// @MainActor
func globalFn() {}
func takesMainActor(_: @MainActor () -> Void) {}

// Check that a member declared in an extension of nonisolated type is considered @MainActor isolated
extension S { // S is nonisolated because `P` is `Sendable`.
  func test() {
    globalFn() // Ok
  }
}

// @MainActor
protocol W {
  func mainActorWitness()
}

extension S: W {
  func mainActorWitness() {} // Ok (even though S is nonisolated new member is `@MainActor`)

  func testIsolation(v: S) {
    takesMainActor(v.mainActorWitness) // Ok
  }
}
