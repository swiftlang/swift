// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift -swift-version 5 -Onone -load-plugin-library %t/%target-library-name(MacroDefinition) %t/enums.swift %t/other.swift %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

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

public struct AddBoolCaseMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["case custom(Bool)"]
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


@attached(member, names: named(custom))
macro AddBoolCase() = #externalMacro(module: "MacroDefinition", type: "AddBoolCaseMacro")

@AddBoolCase
enum BoolKey { case first }

func recognizesFirst(_ key: BoolKey) -> Bool {
  switch key {
  case .first: return true
  case .custom: return false
  }
}

// Look up each generated case here so this file sees the complete enum.
func referenceSizes() -> [Int] {
  _ = NamedKey.custom
  _ = ArbitraryKey.custom
  return [MemoryLayout<NamedKey>.size, MemoryLayout<ArbitraryKey>.size]
}

//--- other.swift
func makeFirst() -> BoolKey { .first }

func consumerSizes() -> [Int] {
  return [MemoryLayout<NamedKey>.size, MemoryLayout<ArbitraryKey>.size]
}

//--- main.swift
// CHECK: layouts match: true
print("layouts match: \(referenceSizes() == consumerSizes())")
// CHECK: first preserved: true
print("first preserved: \(recognizesFirst(makeFirst()))")
