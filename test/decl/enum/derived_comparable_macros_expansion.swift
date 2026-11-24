// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: Comparable {
  case a
  case b
}

// CHECK:      @_implements(Comparable, <(_:_:))
// CHECK-NEXT: static func __derived_enum_less_than(_ lhs: Self, _ rhs: Self) -> Bool {
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
// CHECK-NEXT:   return index_lhs < index_rhs
// CHECK-NEXT: }

enum HasAssociatedValues: Comparable {
  case a(Int)
  case b(String)
  case c
}

// CHECK:       @_implements(Comparable, <(_:_:))
// CHECK-NEXT:  static func __derived_enum_less_than(_ lhs: Self, _ rhs: Self) -> Bool {
// CHECK-NEXT:    switch (lhs, rhs) {
// CHECK-NEXT:    case (.a(let l0), .a(let r0)):
// CHECK-NEXT:      guard l0 == r0 else {
// CHECK-NEXT:      return l0 < r0
// CHECK-NEXT:    }
// CHECK-NEXT:    return false
// CHECK-NEXT:    case (.b(let l0), .b(let r0)):
// CHECK-NEXT:      guard l0 == r0 else {
// CHECK-NEXT:      return l0 < r0
// CHECK-NEXT:    }
// CHECK-NEXT:    return false
// CHECK-NEXT:    case (.c, .c):
// CHECK-EMPTY:
// CHECK-NEXT:        return false
// CHECK-NEXT:    default:
// CHECK-NEXT:      var index_lhs: Swift::Int
// CHECK-NEXT:    switch lhs {
// CHECK-NEXT:    case .a:
// CHECK-NEXT:      index_lhs = 0
// CHECK-NEXT:    case .b:
// CHECK-NEXT:      index_lhs = 1
// CHECK-NEXT:    case .c:
// CHECK-NEXT:      index_lhs = 2
// CHECK-NEXT:    }
// CHECK-NEXT:    var index_rhs: Swift::Int
// CHECK-NEXT:    switch rhs {
// CHECK-NEXT:    case .a:
// CHECK-NEXT:      index_rhs = 0
// CHECK-NEXT:    case .b:
// CHECK-NEXT:      index_rhs = 1
// CHECK-NEXT:    case .c:
// CHECK-NEXT:      index_rhs = 2
// CHECK-NEXT:    }
// CHECK-NEXT:    return index_lhs < index_rhs
// CHECK-NEXT:    }
// CHECK-NEXT:  }

@available(*, unavailable)
enum UnavailableEnum: Comparable {
  case a
  case b
}

// CHECK:      @_implements(Comparable, <(_:_:))
// CHECK-NEXT: static func __derived_enum_less_than(_ lhs: Self, _ rhs: Self) -> Bool {
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
// CHECK-NEXT:   return index_lhs < index_rhs
// CHECK-NEXT: }

enum WithRawIdentifiers: Comparable {
  case `foo bar`
  case `default`(Int)
  case a(`foo bar`: String)
}

// CHECK:       @_implements(Comparable, <(_:_:))
// CHECK-NEXT:  static func __derived_enum_less_than(_ lhs: Self, _ rhs: Self) -> Bool {
// CHECK-NEXT:    switch (lhs, rhs) {
// CHECK-NEXT:    case (.`foo bar`, .`foo bar`):
// CHECK-EMPTY:
// CHECK-NEXT:        return false
// CHECK-NEXT:    case (.`default`(let l0), .`default`(let r0)):
// CHECK-NEXT:      guard l0 == r0 else {
// CHECK-NEXT:      return l0 < r0
// CHECK-NEXT:    }
// CHECK-NEXT:    return false
// CHECK-NEXT:    case (.a(`foo bar`: let l0), .a(`foo bar`: let r0)):
// CHECK-NEXT:      guard l0 == r0 else {
// CHECK-NEXT:      return l0 < r0
// CHECK-NEXT:    }
// CHECK-NEXT:    return false
// CHECK-NEXT:    default:
// CHECK-NEXT:      var index_lhs: Swift::Int
// CHECK-NEXT:    switch lhs {
// CHECK-NEXT:    case .`foo bar`:
// CHECK-NEXT:      index_lhs = 0
// CHECK-NEXT:    case .`default`:
// CHECK-NEXT:      index_lhs = 1
// CHECK-NEXT:    case .a:
// CHECK-NEXT:      index_lhs = 2
// CHECK-NEXT:    }
// CHECK-NEXT:      var index_rhs: Swift::Int
// CHECK-NEXT:    switch rhs {
// CHECK-NEXT:    case .`foo bar`:
// CHECK-NEXT:      index_rhs = 0
// CHECK-NEXT:    case .`default`:
// CHECK-NEXT:      index_rhs = 1
// CHECK-NEXT:    case .a:
// CHECK-NEXT:      index_rhs = 2
// CHECK-NEXT:    }
// CHECK-NEXT:      return index_lhs  < index_rhs
// CHECK-NEXT:    }
// CHECK-NEXT:  }

enum Empty: Comparable {}

// CHECK:       @_implements(Comparable, <(_:_:))
// CHECK-NEXT:  static func __derived_enum_less_than(_ lhs: Self, _ rhs: Self) -> Bool {
// CHECK-EMPTY: 
// CHECK-NEXT:  }
