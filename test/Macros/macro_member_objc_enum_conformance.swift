// REQUIRES: swift_swift_parser, objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -primary-file %t/other.swift %t/enums.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct AddFactoryMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["static func custom() -> Self { .first }"]
  }
}


//--- enums.swift
protocol Marker {}
@attached(member, conformances: Marker, names: named(custom))
macro Inspect() = #externalMacro(module: "MacroDefinition", type: "AddFactoryMacro")
@Inspect
@objc enum Variant: Int, Error { case first }

//--- other.swift
func use() -> Variant { Variant.custom() }
