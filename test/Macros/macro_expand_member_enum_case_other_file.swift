// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// A member macro adds a case with a non-trivial payload. When another file is
// the primary file, lowering the enum must still see that case, so the enum
// is not treated as trivial.
// RUN: %target-swift-emit-silgen -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name Test -primary-file %t/other.swift %t/enums.swift | %FileCheck %t/other.swift

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -verify %t/conformances.swift

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -verify -primary-file %t/bitwise.swift %t/enums.swift

//--- bitwise.swift
func requireBits<T: BitwiseCopyable>(_ type: T.Type) {} // expected-note {{where 'T' =}}
func invalidClosure() -> () -> Int {
  { construct() } // expected-error {{cannot convert value of type 'GeneratedOnly' to closure result type 'Int'}}
}
func checkPayload() {
  requireBits(NamedKey.self) // expected-error {{requires that 'NamedKey' conform to 'BitwiseCopyable'}}
}

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

@AddPayloadCaseNamed
enum GeneratedOnly {}
func construct() -> GeneratedOnly { .custom("hello") }

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

// CHECK-LABEL: sil hidden [ossa] {{.*}}eraseGenerated
// CHECK: apply
// CHECK-NOT: unreachable
// CHECK: init_existential_addr
// CHECK: return
func eraseGenerated() -> Any { construct() }

//--- conformances.swift
protocol Marker {}
@attached(member, conformances: Marker, names: named(custom))
macro WithMarker() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")
@attached(member, names: named(custom))
macro WithoutMarker() = #externalMacro(module: "MacroDefinition", type: "AddPayloadCaseMacro")

enum Written { case plain, custom(String) }
@WithMarker enum GeneratedWithMarker { case plain }
@WithoutMarker enum GeneratedWithoutMarker { case plain }
@WithMarker enum ExplicitlyHashable: Hashable { case plain }
enum Simple { case plain }

func requireHash<T: Hashable>(_ type: T.Type) {} // expected-note 3 {{where 'T' =}}
func check() {
  requireHash(Written.self) // expected-error {{requires that 'Written' conform to 'Hashable'}}
  requireHash(GeneratedWithMarker.self) // expected-error {{requires that 'GeneratedWithMarker' conform to 'Hashable'}}
  requireHash(GeneratedWithoutMarker.self) // expected-error {{requires that 'GeneratedWithoutMarker' conform to 'Hashable'}}
  requireHash(ExplicitlyHashable.self)
  requireHash(Simple.self)
}
