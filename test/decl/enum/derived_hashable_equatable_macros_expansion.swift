// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: Hashable {
  case a
  case b
}

// CHECK: @_semantics("derived_enum_equals")
// CHECK-NEXT: @_implements(Swift::Equatable, ==(_:_:))
// CHECK-NEXT: static func __derived_enum_equals(_ lhs: Self, _ rhs: Self) -> Swift::Bool {
// CHECK-NEXT:   var index_lhs: Swift::Int
// CHECK-NEXT:   switch lhs {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:     index_lhs = 0
// CHECK-NEXT:   case .b:
// CHECK-NEXT:     index_lhs = 1
// CHECK-NEXT:   }
// CHECK-NEXT:   var index_rhs: Swift::Int
// CHECK-NEXT:   switch rhs {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:     index_rhs = 0
// CHECK-NEXT:   case .b:
// CHECK-NEXT:     index_rhs = 1
// CHECK-NEXT:   }
// CHECK-NEXT:   return index_lhs == index_rhs
// CHECK-NEXT: }

// CHECK: var hashValue: Swift::Int {
// CHECK-NEXT:   return Swift::_hashValue(for: self)
// CHECK-NEXT: }

//CHECK: func hash(into hasher: inout Swift::Hasher) {
//CHECK-NEXT:   var discriminator: Swift::Int
//CHECK-NEXT:   switch self {
//CHECK-NEXT:   case .a:
//CHECK-NEXT:     discriminator = 0
//CHECK-NEXT:   case .b:
//CHECK-NEXT:     discriminator = 1
//CHECK-NEXT:   }
//CHECK-NEXT:   hasher.combine(discriminator)
//CHECK-NEXT: }

enum WithValues: Hashable {
  case a(Int)
  case b(String)
  case c
}

// CHECK: var hashValue: Swift::Int {
// CHECK-NEXT:   return Swift::_hashValue(for: self)
// CHECK-NEXT: }

// CHECK: func hash(into hasher: inout Swift::Hasher) {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .a(let a0):
// CHECK-NEXT:     hasher.combine(0)
// CHECK-NEXT:     hasher.combine(a0)
// CHECK-NEXT:   case .b(let a0):
// CHECK-NEXT:     hasher.combine(1)
// CHECK-NEXT:     hasher.combine(a0)
// CHECK-NEXT:   case .c:
// CHECK-NEXT:     hasher.combine(2)
// CHECK-NEXT:   }
// CHECK-NEXT: }

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
// CHECK-NEXT: }

enum MultipleValues: Hashable {
  case a(Int, String)
  case b(Bool)
}

// CHECK: var hashValue: Swift::Int {
// CHECK-NEXT:   return Swift::_hashValue(for: self)
// CHECK-NEXT: }

//CHECK: func hash(into hasher: inout Swift::Hasher) {
//CHECK-NEXT:   switch self {
//CHECK-NEXT:   case .a(let a0, let a1):
//CHECK-NEXT:     hasher.combine(0)
//CHECK-NEXT:     hasher.combine(a0)
//CHECK-NEXT:     hasher.combine(a1)
//CHECK-NEXT:   case .b(let a0):
//CHECK-NEXT:     hasher.combine(1)
//CHECK-NEXT:     hasher.combine(a0)
//CHECK-NEXT:   }
//CHECK-NEXT: }

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

//CHECK: var hashValue: Swift::Int {
//CHECK-NEXT:   return Swift::_hashValue(for: self)
//CHECK-NEXT: }

//CHECK: func hash(into hasher: inout Swift::Hasher) {
//CHECK-NEXT:   switch self {
//CHECK-NEXT:   case .a(x: let a0, y: let a1):
//CHECK-NEXT:     hasher.combine(0)
//CHECK-NEXT:     hasher.combine(a0)
//CHECK-NEXT:     hasher.combine(a1)
//CHECK-NEXT:   case .b(value: let a0):
//CHECK-NEXT:     hasher.combine(1)
//CHECK-NEXT:     hasher.combine(a0)
//CHECK-NEXT:   }
//CHECK-NEXT: }

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

// CHECK: var hashValue: Swift::Int {
// CHECK-NEXT:   return Swift::_hashValue(for: self)
// CHECK-NEXT: }

// CHECK: func hash(into hasher: inout Swift::Hasher) {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .`foo bar`:
// CHECK-NEXT:     hasher.combine(0)
// CHECK-NEXT:   case .`default`(let a0):
// CHECK-NEXT:     hasher.combine(1)
// CHECK-NEXT:     hasher.combine(a0)
// CHECK-NEXT:   case .a(`foo bar`: let a0):
// CHECK-NEXT:     hasher.combine(2)
// CHECK-NEXT:     hasher.combine(a0)
// CHECK-NEXT:   }
// CHECK-NEXT: }

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
