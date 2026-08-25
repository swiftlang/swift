// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: Hashable {
  case a
  case b
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK: @_implements(Swift::Equatable, ==(_:_:))
// CHECK: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK:   var index_lhs: Swift::Int
// CHECK:   switch lhs {
// CHECK:   case .a:
// CHECK:     index_lhs = 0
// CHECK:   case .b:
// CHECK:     index_lhs = 1
// CHECK:   }
// CHECK:   var index_rhs: Swift::Int
// CHECK:   switch rhs {
// CHECK:   case .a:
// CHECK:     index_rhs = 0
// CHECK:   case .b:
// CHECK:     index_rhs = 1
// CHECK:   }
// CHECK:   return index_lhs == index_rhs
// CHECK: }

enum WithValues: Hashable {
  case a(Int)
  case b(String)
  case c
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// CHECK-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK-NEXT:   switch (lhs, rhs) {
// CHECK-NEXT:   case (.a(let l0), .a(let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.b(let l0), .b(let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.c, .c):
// CHECK-NEXT:     return true
// CHECK-NEXT:   default:
// CHECK-NEXT:       return false
// CHECK-NEXT:   }
// CHECK-NEX: }

enum MultipleValues: Hashable {
  case a(Int, String)
  case b(Bool)
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// CHECK-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK-NEXT:   switch (lhs, rhs) {
// CHECK-NEXT:   case (.a(let l0, let l1), .a(let r0, let r1)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     guard l1 == r1 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.b(let l0), .b(let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   default:
// CHECK-NEXT:       return false
// CHECK-NEXT:   }
// CHECK-NEXT: }

enum WithLabels: Hashable {
  case a(x: Int, y: String)
  case b(value: Bool)
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// CHECK-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK-NEXT:   switch (lhs, rhs) {
// CHECK-NEXT:   case (.a(x: let l0, y: let l1), .a(x: let r0, y: let r1)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     guard l1 == r1 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.b(value: let l0), .b(value: let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   default:
// CHECK-NEXT:       return false
// CHECK-NEXT:   }
// CHECK-NEXT: }

enum WithRawIdentifiers: Hashable {
  case `foo bar`
  case `default`(Int)
  case a(`foo bar`: String)
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// CHECK-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK-NEXT:   switch (lhs, rhs) {
// CHECK-NEXT:   case (.`foo bar`, .`foo bar`):
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.`default`(let l0), .`default`(let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   case (.a(`foo bar`: let l0), .a(`foo bar`: let r0)):
// CHECK-NEXT:     guard l0 == r0 else {
// CHECK-NEXT:       return false
// CHECK-NEXT:     }
// CHECK-NEXT:     return true
// CHECK-NEXT:   default:
// CHECK-NEXT:       return false
// CHECK-NEXT:   }
// CHECK-NEXT: }
