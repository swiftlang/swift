// REQUIRES: swift_swift_parser
// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the macro library
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift

// RUN: %target-swift-frontend -emit-sil -swift-version 5 -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/main_actor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -emit-sil -swift-version 5 -strict-concurrency=complete -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/main_actor.swift | %FileCheck --check-prefix=MAIN %s
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/main_actor.swift | %FileCheck --check-prefix=MAIN %s

// RUN: %target-swift-frontend -emit-sil -swift-version 5 -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/nonisolated_override.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -emit-sil -swift-version 5 -strict-concurrency=complete -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/nonisolated_override.swift | %FileCheck --check-prefix=NONISO %s
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -parse-as-library -enable-experimental-feature DefaultIsolationPerFile -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name M %t/nonisolated_override.swift | %FileCheck --check-prefix=NONISO %s

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

// Emits `<isolation> extension <Type> { func generated() {} }`.
public struct AddExtMacro: ExtensionMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    var isolation = ""
    if let arg = node.arguments?.as(LabeledExprListSyntax.self)?.first?.expression,
       let literal = arg.as(StringLiteralExprSyntax.self) {
      isolation = literal.representedLiteralValue ?? ""
    }
    let ext: DeclSyntax =
      """
      \(raw: isolation) extension \(type.trimmed) {
        func generated() {}
      }
      """
    return [ext.cast(ExtensionDeclSyntax.self)]
  }
}

//--- main_actor.swift
using @MainActor

@attached(extension, names: named(generated))
macro AddExt(_ isolation: String = "") = #externalMacro(module: "MacroDefinition", type: "AddExtMacro")

// The macro-synthesized extension inherits the file default.
// MAIN: @MainActor extension Defaulted
@AddExt
struct Defaulted {}

// Explicit isolation in the macro's output survives the file default.
// MAIN: nonisolated extension Explicit
@AddExt("nonisolated")
struct Explicit {}

// MAIN: // Defaulted.generated()
// MAIN-NEXT: // Isolation: global_actor. type: MainActor

// MAIN: // Explicit.generated()
// MAIN-NEXT: // Isolation: nonisolated

//--- nonisolated_override.swift
using nonisolated

@attached(extension, names: named(generated))
macro AddExt(_ isolation: String = "") = #externalMacro(module: "MacroDefinition", type: "AddExtMacro")

// The file default applies, overriding the extended type's isolation.
// NONISO: nonisolated extension Defaulted
@MainActor
@AddExt
struct Defaulted {}

// Explicit isolation in the macro's output survives the file default.
// NONISO: @MainActor extension Explicit
@AddExt("@MainActor")
struct Explicit {}

// NONISO: // Defaulted.generated()
// NONISO-NEXT: // Isolation: nonisolated

// NONISO: // Explicit.generated()
// NONISO-NEXT: // Isolation: global_actor. type: MainActor
