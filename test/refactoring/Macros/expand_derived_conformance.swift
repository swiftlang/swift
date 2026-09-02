// REQUIRES: swift_feature_DeriveConformancesViaMacros

// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) --dump-text -source-filename %s -pos=%(line+1):21 | %FileCheck -check-prefix=DIRECT %s
struct Direct: Equatable {
  var x: Int = 0
}
// DIRECT: {{.*}}.swift [[@LINE-3]]:27 -> [[@LINE-3]]:27
// DIRECT-EMPTY:
// DIRECT-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// DIRECT-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=%(line+5):28 | %FileCheck -check-prefix=IN_EXTENSION %s
struct InExtension {
  var x: Int = 0
}

extension InExtension: Equatable {}
// IN_EXTENSION: {{.*}}.swift [[@LINE-1]]:35 -> [[@LINE-1]]:35
// IN_EXTENSION-EMPTY:
// IN_EXTENSION-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// IN_EXTENSION-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {

typealias EquatableAndHashable = Equatable & Hashable

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=%(line+1):31 | %FileCheck -check-prefix=VIA_TYPEALIAS %s
struct ViaTypealias: EquatableAndHashable {
  var x: Int = 0
}
// VIA_TYPEALIAS: {{.*}}.swift [[@LINE-3]]:44 -> [[@LINE-3]]:44
// VIA_TYPEALIAS-EMPTY:
// VIA_TYPEALIAS-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// VIA_TYPEALIAS-NEXT: static func __derived_struct_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// VIA_TYPEALIAS-NOT: {{.*}}.swift [[@LINE-7]]:44 -> [[@LINE-7]]:44

// RUN: %refactor-check-compiles -expand-derived-conformance -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -source-filename %s -pos=%(line+1):17 | %FileCheck -check-prefix=ENUM %s
enum Enum: Hashable {
  case a
  case b(Int)
}
// ENUM: {{.*}}.swift [[@LINE-4]]:22 -> [[@LINE-4]]:22
// ENUM-EMPTY:
// ENUM-NEXT: @_semantics("derived_enum_equals")
// ENUM-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// ENUM-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
