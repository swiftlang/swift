// REQUIRES: swift_feature_DeriveConformancesViaMacros

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

// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) --dump-text -source-filename %s -pos=3:21 | %FileCheck -check-prefix=DIRECT %s
// DIRECT: {{.*}}.swift 3:27 -> 3:27
// DIRECT-EMPTY:
// DIRECT-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// DIRECT-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=11:28 | %FileCheck -check-prefix=IN_EXTENSION %s
// IN_EXTENSION: {{.*}}.swift 11:35 -> 11:35
// IN_EXTENSION-EMPTY:
// IN_EXTENSION-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// IN_EXTENSION-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=15:31 | %FileCheck -check-prefix=VIA_TYPEALIAS %s
// VIA_TYPEALIAS: {{.*}}.swift 15:44 -> 15:44
// VIA_TYPEALIAS-EMPTY:
// VIA_TYPEALIAS-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// VIA_TYPEALIAS-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// VIA_TYPEALIAS-NOT: {{.*}}.swift 15:44 -> 15:44

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=19:17 | %FileCheck -check-prefix=ENUM %s
// ENUM: {{.*}}.swift 19:22 -> 19:22
// ENUM-EMPTY:
// ENUM-NEXT: @_semantics("derived_enum_equals")
// ENUM-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// ENUM-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
