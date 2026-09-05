// REQUIRES: swift_swift_parser, asserts
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+1):16 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=DIRECT %s
struct Direct: Equatable {
  var x: Int = 0
}
// DIRECT: source.edit.kind.active:
// DIRECT-NEXT: {{^}}{{ +}}[[@LINE-4]]:27-[[@LINE-4]]:27 ({{.*}}_deriveEquatablefMf_.swift) "
// DIRECT-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// DIRECT-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+4):24 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=IN_EXTENSION %s
struct InExtension {
  var x: Int = 0
}
extension InExtension: Equatable {}
// IN_EXTENSION: source.edit.kind.active:
// IN_EXTENSION-NEXT: {{^}}{{ +}}[[@LINE-2]]:35-[[@LINE-2]]:35 ({{.*}}_deriveEquatablefMf_.swift) "
// IN_EXTENSION-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// IN_EXTENSION-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

typealias EquatableAndHashable = Equatable & Hashable

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+1):22 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=VIA_TYPEALIAS %s
struct ViaTypealias: EquatableAndHashable {
  var x: Int = 0
}
// VIA_TYPEALIAS: source.edit.kind.active:
// VIA_TYPEALIAS-NEXT: {{^}}{{ +}}[[@LINE-4]]:44-[[@LINE-4]]:44 ({{.*}}_deriveHashablefMf_.swift) "
// VIA_TYPEALIAS-NEXT: var hashValue: Swift::Int {
// VIA_TYPEALIAS: source.edit.kind.active:
// VIA_TYPEALIAS-NEXT: {{^}}{{ +}}[[@LINE-7]]:44-[[@LINE-7]]:44 ({{.*}}_deriveHashablefMf0_.swift) "
// VIA_TYPEALIAS-NEXT: func hash(into hasher: inout Swift::Hasher) {
// VIA_TYPEALIAS-NEXT: hasher.combine(self.x)
// VIA_TYPEALIAS: source.edit.kind.active:
// VIA_TYPEALIAS-NEXT: {{^}}{{ +}}[[@LINE-11]]:44-[[@LINE-11]]:44 ({{.*}}_deriveEquatablefMf_.swift) "
// VIA_TYPEALIAS-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// VIA_TYPEALIAS-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// VIA_TYPEALIAS-NOT: source.edit.kind.active

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+1):12 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck -check-prefix=ENUM %s
enum Enum: Hashable {
  case a
  case b(Int)
}
// ENUM: source.edit.kind.active:
// ENUM-NEXT: {{^}}{{ +}}[[@LINE-5]]:22-[[@LINE-5]]:22 ({{.*}}_deriveHashablefMf_.swift) "
// ENUM-NEXT: var hashValue: Swift::Int {
// ENUM: source.edit.kind.active:
// ENUM-NEXT: {{^}}{{ +}}[[@LINE-8]]:22-[[@LINE-8]]:22 ({{.*}}_deriveHashablefMf0_.swift) "
// ENUM-NEXT: func hash(into hasher: inout Swift::Hasher) {
// ENUM-NEXT: switch self {
// ENUM-NEXT: case .a:
// ENUM-NEXT: hasher.combine(0)
// ENUM-NEXT: case .b(let a0):
// ENUM-NEXT: hasher.combine(1)
// ENUM-NEXT: hasher.combine(a0)
// ENUM: source.edit.kind.active:
// ENUM-NEXT: {{^}}{{ +}}[[@LINE-17]]:22-[[@LINE-17]]:22 ({{.*}}_deriveEquatablefMf_.swift) "
// ENUM-NEXT: @_semantics("derived_enum_equals")
// ENUM-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// ENUM-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// ENUM-NOT: source.edit.kind.active

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+2):16 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck --allow-empty -check-prefix=NO_EXPANSION %s
class Base {}
class Derived: Base {}

// RUN: %sourcekitd-test -req=refactoring.expand.derived_conformance -pos=%(line+1):21 %s -- -enable-experimental-feature DeriveConformancesViaMacros -module-name DerivedConformanceUser %s | %FileCheck --allow-empty -check-prefix=NO_EXPANSION %s
struct Suppressed: ~Copyable {
  var x: Int = 0
}

// NO_EXPANSION-NOT: source.edit.kind.active
