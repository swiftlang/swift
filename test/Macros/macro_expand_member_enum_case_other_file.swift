// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// A member macro adds a case with a non-trivial payload. When another file is
// the primary file, lowering the enum must still see that case, so the enum
// is not treated as trivial.
// RUN: %target-swift-emit-silgen -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name Test -primary-file %t/other.swift %t/enums.swift | %FileCheck %t/other.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct AddPayloadCaseMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["case custom(String)"]
  }
}

//--- enums.swift
@attached(member, names: named(custom))
macro AddPayloadCaseNamed() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")

@attached(member, names: arbitrary)
macro AddPayloadCaseArbitrary() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")

@AddPayloadCaseNamed
enum NamedKey {
  case first
  case second
}

@AddPayloadCaseArbitrary
enum ArbitraryKey {
  case first
  case second
}

//--- other.swift
// This file never mentions the macro-introduced `custom` case.
struct NamedHolder {
  var key: NamedKey
}

struct ArbitraryHolder {
  var key: ArbitraryKey
}

// CHECK-LABEL: sil hidden [ossa] @$s4Test11NamedHolderV3keyAcA0B3KeyO_tcfC :
// CHECK: bb0(%0 : @owned $NamedKey, %1 : $@thin NamedHolder.Type):

// CHECK-LABEL: sil hidden [ossa] @$s4Test15ArbitraryHolderV3keyAcA0B3KeyO_tcfC :
// CHECK: bb0(%0 : @owned $ArbitraryKey, %1 : $@thin ArbitraryHolder.Type):
