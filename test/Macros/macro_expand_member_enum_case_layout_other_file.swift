// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift -swift-version 5 -Onone -no-whole-module-optimization -disable-batch-mode -load-plugin-library %t/%target-library-name(MacroDefinition) %t/enums.swift %t/other.swift %t/main.swift -o %t/main
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

public struct ConditionalCaseMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    protocols.isEmpty ? [] : ["case custom"]
  }
}

public struct ObserveEquatableMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    protocols.isEmpty ? ["static var marker: Bool { true }"] : []
  }
}

//--- enums.swift
@attached(member, names: named(custom))
macro AddPayloadCaseNamed() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")

@attached(member, names: arbitrary)
macro AddPayloadCaseArbitrary() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")

@AddPayloadCaseNamed enum Message {}
func constructMessage() -> Message { .custom("hello") }

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

@attached(member, conformances: Equatable, names: named(custom))
macro ConditionalCase() = #externalMacro(module: "MacroDefinition", type: "ConditionalCaseMacro")

@ConditionalCase enum Variant { case first }
enum WrittenVariant { case first }
@ConditionalCase enum WithPayload { case payload(Int) }
func generatedCase() -> WithPayload { .custom }
func declarationSize() -> Int { MemoryLayout<Variant>.size }

protocol Marker {}
@attached(member, conformances: Marker, names: named(custom))
macro IntroduceCase() = #externalMacro(module: "MacroDefinition", type: "ConditionalCaseMacro")

// The overlapping name can trigger extension expansion during case lookup.
@attached(member, conformances: Equatable, names: named(marker), named(custom))
macro ObserveEquatable() = #externalMacro(module: "MacroDefinition", type: "ObserveEquatableMacro")

@IntroduceCase enum GeneratedOnly {}
enum WrittenOnly { case custom }
@ObserveEquatable extension GeneratedOnly {}
@ObserveEquatable extension WrittenOnly {}

func declarationMarkers() -> Bool {
  _ = GeneratedOnly.custom
  return GeneratedOnly.marker && WrittenOnly.marker
}

@attached(member, conformances: BitwiseCopyable, names: named(custom))
macro IfMissingBitwise() = #externalMacro(module: "MacroDefinition", type: "ConditionalCaseMacro")

@IfMissingBitwise enum Bits { case first(Int) }
enum WrittenBits { case first(Int), custom }
@IfMissingBitwise enum ExplicitBits: BitwiseCopyable { case first(Int) }
func checkExplicit(_ bits: ExplicitBits) {
  switch bits { case .first: break }
}
func declaredBitsSize() -> Int {
  _ = Bits.custom
  return MemoryLayout<Bits>.size
}

// Look up each generated case here so this file sees the complete enum.
func referenceSizes() -> [Int] {
  _ = NamedKey.custom
  _ = ArbitraryKey.custom
  return [MemoryLayout<NamedKey>.size, MemoryLayout<ArbitraryKey>.size]
}

//--- other.swift
func eraseMessage() -> Any { constructMessage() }

func requireBits<T: BitwiseCopyable>(_ type: T.Type) {}
func consumerBitsSize() -> Int {
  requireBits(Bits.self)
  return MemoryLayout<Bits>.size
}

func consumerMarkers() -> Bool {
  _ = GeneratedOnly.custom
  return GeneratedOnly.marker && WrittenOnly.marker
}

func consumerSize() -> Int { MemoryLayout<Variant>.size }
func makeFirst() -> BoolKey { .first }

func consumerSizes() -> [Int] {
  return [MemoryLayout<NamedKey>.size, MemoryLayout<ArbitraryKey>.size]
}

//--- main.swift
// CHECK: layouts match: true
print("layouts match: \(referenceSizes() == consumerSizes())")
// CHECK: first preserved: true
print("first preserved: \(recognizesFirst(makeFirst()))")
// CHECK: conditional expansion matches written enum: true
let expected = MemoryLayout<WrittenVariant>.size
print("conditional expansion matches written enum: \(declarationSize() == expected && consumerSize() == expected)")

// CHECK: extension markers match: true
print("extension markers match: \(declarationMarkers() && consumerMarkers())")

// CHECK: bitwise expansion matches written cases: true
let bitsSize = MemoryLayout<WrittenBits>.size
print("bitwise expansion matches written cases: \(declaredBitsSize() == bitsSize && consumerBitsSize() == bitsSize)")

// CHECK: erased payload: hello
if let message = eraseMessage() as? Message, case let .custom(payload) = message {
  print("erased payload: \(payload)")
}
