// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -print-ast %s > %t/legacy.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,CASE < %t/legacy.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,EQ,EQ_LEGACY < %t/legacy.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,LT < %t/legacy.txt
// RUN: %target-swift-frontend -print-ast %s -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros > %t/macros.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,CASE < %t/macros.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,EQ,EQ_MACRO < %t/macros.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,LT,LT_MACRO < %t/macros.txt

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : Comparable
enum Simple: Comparable {
  // CASE:        case a
  case a
  // CASE:        case b
  case b

  // EQ:           @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ [[A:a|lhs]]: {{Simple|`Self`}}, _ [[B:b|rhs]]: {{Simple|`Self`}}) -> Bool {
  // EQ-NEXT:        var index_[[A]]: Int
  // EQ_MACRO-EMPTY:
  // EQ-NEXT:        switch [[A]] {
  // EQ-NEXT:        case .a:
  // EQ-NEXT:          index_[[A]] = 0
  // EQ-NEXT:        case .b:
  // EQ-NEXT:          index_[[A]] = 1
  // EQ-NEXT:        }
  // EQ-NEXT:        var index_[[B]]: Int
  // EQ_MACRO-EMPTY:
  // EQ-NEXT:        switch [[B]] {
  // EQ-NEXT:        case .a:
  // EQ-NEXT:          index_[[B]] = 0
  // EQ-NEXT:        case .b:
  // EQ-NEXT:          index_[[B]] = 1
  // EQ-NEXT:        }
  // EQ-NEXT:        return index_[[A]] == index_[[B]]
  // EQ-NEXT:      }

  // LT:           @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ [[A:a|lhs]]: {{Simple|`Self`}}, _ [[B:b|rhs]]: {{Simple|`Self`}}) -> Bool {
  // LT-NEXT:        var index_[[A]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:        switch [[A]] {
  // LT-NEXT:        case .a:
  // LT-NEXT:          index_[[A]] = 0
  // LT-NEXT:        case .b:
  // LT-NEXT:          index_[[A]] = 1
  // LT-NEXT:        }
  // LT-NEXT:        var index_[[B]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:        switch [[B]] {
  // LT-NEXT:        case .a:
  // LT-NEXT:          index_[[B]] = 0
  // LT-NEXT:        case .b:
  // LT-NEXT:          index_[[B]] = 1
  // LT-NEXT:        }
  // LT-NEXT:        return index_[[A]] < index_[[B]]
  // LT-NEXT:      }
}

// CHECK-LABEL: internal enum HasAssociatedValues : Comparable
enum HasAssociatedValues: Comparable {
  // CASE:        case a(Int)
  case a(Int)
  // CASE:        case b(String)
  case b(String)
  // CASE:        case c
  case c

  // EQ:           @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ [[A:a|lhs]]: {{HasAssociatedValues|`Self`}}, _ [[B:b|rhs]]: {{HasAssociatedValues|`Self`}}) -> Bool {
  // EQ-NEXT:        switch ([[A]], [[B]]) {
  // EQ-NEXT:        case (.a(let l0), .a(let r0)):
  // EQ-NEXT:          guard l0 == r0 else {
  // EQ-NEXT:            return false
  // EQ-NEXT:          }
  // EQ-NEXT:          return true
  // EQ-NEXT:        case (.b(let l0), .b(let r0)):
  // EQ-NEXT:          guard l0 == r0 else {
  // EQ-NEXT:            return false
  // EQ-NEXT:          }
  // EQ-NEXT:          return true
  // EQ-NEXT:        case (.c, .c):
  // EQ-NEXT:          return true
  // EQ-NEXT:        default:
  // EQ-NEXT:          return false
  // EQ-NEXT:        }
  // EQ-NEXT:      }

  // LT:           @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ [[A:a|lhs]]: {{HasAssociatedValues|`Self`}}, _ [[B:b|rhs]]: {{HasAssociatedValues|`Self`}}) -> Bool {
  // LT-NEXT:        switch ([[A]], [[B]]) {
  // LT-NEXT:        case (.a(let l0), .a(let r0)):
  // LT-NEXT:          guard l0 == r0 else {
  // LT-NEXT:            return l0 < r0
  // LT-NEXT:          }
  // LT-NEXT:          return false
  // LT-NEXT:        case (.b(let l0), .b(let r0)):
  // LT-NEXT:          guard l0 == r0 else {
  // LT-NEXT:            return l0 < r0
  // LT-NEXT:          }
  // LT-NEXT:          return false
  // LT-NEXT:        case (.c, .c):
  // LT-NEXT:          return false
  // LT-NEXT:        default:
  // LT-NEXT:          var index_[[A]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:          switch [[A]] {
  // LT-NEXT:          case .a:
  // LT-NEXT:            index_[[A]] = 0
  // LT-NEXT:          case .b:
  // LT-NEXT:            index_[[A]] = 1
  // LT-NEXT:          case .c:
  // LT-NEXT:            index_[[A]] = 2
  // LT-NEXT:          }
  // LT-NEXT:          var index_[[B]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:          switch [[B]] {
  // LT-NEXT:          case .a:
  // LT-NEXT:            index_[[B]] = 0
  // LT-NEXT:          case .b:
  // LT-NEXT:            index_[[B]] = 1
  // LT-NEXT:          case .c:
  // LT-NEXT:            index_[[B]] = 2
  // LT-NEXT:          }
  // LT-NEXT:          return index_[[A]] < index_[[B]]
  // LT-NEXT:        }
  // LT-NEXT:      }
}

// CHECK-LABEL: internal enum UnavailableEnum : Comparable
@available(*, unavailable)
enum UnavailableEnum: Comparable {
  // CASE:        case a
  case a
  // CASE:        case b
  case b

  // EQ:           @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ [[A:a|lhs]]: {{UnavailableEnum|`Self`}}, _ [[B:b|rhs]]: {{UnavailableEnum|`Self`}}) -> Bool {
  // EQ-NEXT:        var index_[[A]]: Int
  // EQ_MACRO-EMPTY:
  // EQ-NEXT:        switch [[A]] {
  // EQ-NEXT:        case .a:
  // EQ-NEXT:          index_[[A]] = 0
  // EQ-NEXT:        case .b:
  // EQ-NEXT:          index_[[A]] = 1
  // EQ-NEXT:        }
  // EQ-NEXT:        var index_[[B]]: Int
  // EQ_MACRO-EMPTY:
  // EQ-NEXT:        switch [[B]] {
  // EQ-NEXT:        case .a:
  // EQ-NEXT:          index_[[B]] = 0
  // EQ-NEXT:        case .b:
  // EQ-NEXT:          index_[[B]] = 1
  // EQ-NEXT:        }
  // EQ-NEXT:        return index_[[A]] == index_[[B]]
  // EQ-NEXT:      }

  // LT:           @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ [[A:a|lhs]]: {{UnavailableEnum|`Self`}}, _ [[B:b|rhs]]: {{UnavailableEnum|`Self`}}) -> Bool {
  // LT-NEXT:        var index_[[A]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:        switch [[A]] {
  // LT-NEXT:        case .a:
  // LT-NEXT:          index_[[A]] = 0
  // LT-NEXT:        case .b:
  // LT-NEXT:          index_[[A]] = 1
  // LT-NEXT:        }
  // LT-NEXT:        var index_[[B]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:        switch [[B]] {
  // LT-NEXT:        case .a:
  // LT-NEXT:          index_[[B]] = 0
  // LT-NEXT:        case .b:
  // LT-NEXT:          index_[[B]] = 1
  // LT-NEXT:        }
  // LT-NEXT:        return index_[[A]] < index_[[B]]
  // LT-NEXT:      }
}

// CHECK-LABEL: internal enum WithRawIdentifiers : Comparable
enum WithRawIdentifiers: Comparable {
  // CASE:        case `foo bar`
  case `foo bar`
  // CASE:        case `default`(Int)
  case `default`(Int)
  // CASE:        case a(`foo bar`: String)
  case a(`foo bar`: String)

  // FIXME: A few of these checks are missing backticks for raw identifiers. This is an
  // ASTPrinter bug.
  
  // EQ:           @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ [[A:a|lhs]]: {{WithRawIdentifiers|`Self`}}, _ [[B:b|rhs]]: {{WithRawIdentifiers|`Self`}}) -> Bool {
  // EQ-NEXT:        switch ([[A]], [[B]]) {
  // EQ-NEXT:        case (.foo bar, .foo bar):
  // EQ-NEXT:          return true
  // EQ-NEXT:        case (.default(let l0), .default(let r0)):
  // EQ-NEXT:          guard l0 == r0 else {
  // EQ-NEXT:            return false
  // EQ-NEXT:          }
  // EQ-NEXT:          return true
  // EQ-NEXT:        case (.a({{(foo bar: )?}}let l0), .a({{(foo bar: )?}}let r0)):
  // EQ-NEXT:          guard l0 == r0 else {
  // EQ-NEXT:            return false
  // EQ-NEXT:          }
  // EQ-NEXT:          return true
  // EQ-NEXT:        default:
  // EQ-NEXT:          return false
  // EQ-NEXT:        }
  // EQ-NEXT:      }

  // LT:           @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ [[A:a|lhs]]: {{WithRawIdentifiers|`Self`}}, _ [[B:b|rhs]]: {{WithRawIdentifiers|`Self`}}) -> Bool {
  // LT-NEXT:        switch ([[A]], [[B]]) {
  // LT-NEXT:        case (.foo bar, .foo bar):
  // LT-NEXT:          return false
  // LT-NEXT:        case (.default(let l0), .default(let r0)):
  // LT-NEXT:          guard l0 == r0 else {
  // LT-NEXT:            return l0 < r0
  // LT-NEXT:          }
  // LT-NEXT:          return false
  // LT-NEXT:        case (.a({{(foo bar: )?}}let l0), .a({{(foo bar: )?}}let r0)):
  // LT-NEXT:          guard l0 == r0 else {
  // LT-NEXT:            return l0 < r0
  // LT-NEXT:          }
  // LT-NEXT:          return false
  // LT-NEXT:        default:
  // LT-NEXT:          var index_[[A]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:          switch [[A]] {
  // LT-NEXT:          case .foo bar:
  // LT-NEXT:            index_[[A]] = 0
  // LT-NEXT:          case .default:
  // LT-NEXT:            index_[[A]] = 1
  // LT-NEXT:          case .a:
  // LT-NEXT:            index_[[A]] = 2
  // LT-NEXT:          }
  // LT-NEXT:          var index_[[B]]: Int
  // LT_MACRO-EMPTY:
  // LT-NEXT:          switch [[B]] {
  // LT-NEXT:          case .foo bar:
  // LT-NEXT:            index_[[B]] = 0
  // LT-NEXT:          case .default:
  // LT-NEXT:            index_[[B]] = 1
  // LT-NEXT:          case .a:
  // LT-NEXT:            index_[[B]] = 2
  // LT-NEXT:          }
  // LT-NEXT:          return index_[[A]] < index_[[B]]
  // LT-NEXT:        }
  // LT-NEXT:      }
}

// CHECK-LABEL: internal enum Empty : Comparable
enum Empty: Comparable {
  // EQ:           @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ [[A:a|lhs]]: {{Empty|`Self`}}, _ [[B:b|rhs]]: {{Empty|`Self`}}) -> Bool {
  // EQ_LEGACY-NEXT:    switch ([[A]], [[B]]) {
  // EQ_LEGACY-NEXT:    }
  // EQ-NEXT:      }

  // LT:           @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ [[A:a|lhs]]: {{Empty|`Self`}}, _ [[B:b|rhs]]: {{Empty|`Self`}}) -> Bool {
  // LT-NEXT:      }
}
