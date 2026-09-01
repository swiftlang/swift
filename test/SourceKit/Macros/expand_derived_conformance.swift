struct Direct: Equatable {
  var x: Int = 0
}

struct InExtension {
  var x: Int = 0
}

extension InExtension: Equatable {}

typealias EquatableAndHashable = Equatable & Hashable

struct ViaTypealias: EquatableAndHashable {
  var x: Int = 0
}

enum Enum: Hashable {
  case a
  case b(Int)
}

// Not a protocol entry: nothing to expand.
class Base {}
class Derived: Base {}

// A suppressed entry is not a derived conformance either.
struct Suppressed: ~Copyable {
  var x: Int = 0
}

// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_DeriveConformancesViaMacros

//##-- Conformance stated in the inheritance clause.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=1:16 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=DIRECT %s
// DIRECT: source.edit.kind.active:
// DIRECT-NEXT: __derivation_macro__Direct@==__buffer 1:1-1:{{[0-9]+}} ({{.*}}_deriveEquatablefMf_.swift) "@_implements(Swift::Equatable, ==(_:_:))
// DIRECT-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

//##-- Conformance stated in an extension.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=9:24 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=IN_EXTENSION %s
// IN_EXTENSION: source.edit.kind.active:
// IN_EXTENSION-NEXT: __derivation_macro__InExtension@==__buffer 1:1-1:{{[0-9]+}} ({{.*}}_deriveEquatablefMf_.swift) "@_implements(Swift::Equatable, ==(_:_:))
// IN_EXTENSION-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

//##-- A typealias naming a protocol composition resolves to the same buffer for both protocols, so only one edit is expected.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=13:22 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=VIA_TYPEALIAS %s
// VIA_TYPEALIAS: source.edit.kind.active:
// VIA_TYPEALIAS-NEXT: __derivation_macro__ViaTypealias@==__buffer 1:1-1:{{[0-9]+}} ({{.*}}_deriveEquatablefMf_.swift) "@_implements(Swift::Equatable, ==(_:_:))
// VIA_TYPEALIAS-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// VIA_TYPEALIAS-NOT: __derivation_macro__

//##-- Hashable has no macro-derived witnesses of its own, so this walks its inherited protocols down to Equatable.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=17:12 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=ENUM %s
// ENUM: source.edit.kind.active:
// ENUM-NEXT: __derivation_macro__Enum@==__buffer 1:1-1:{{[0-9]+}} ({{.*}}_deriveEquatablefMf_.swift) "@_semantics("derived_enum_equals")
// ENUM-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// ENUM-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

//##-- A superclass entry denotes no protocol so do nothing.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=24:16 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck --allow-empty -check-prefix=NO_EXPANSION %s

//##-- A suppressed entry is not a derived conformance so do nothing.
// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=27:21 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck --allow-empty -check-prefix=NO_EXPANSION %s

// NO_EXPANSION-NOT: source.edit.kind.active
