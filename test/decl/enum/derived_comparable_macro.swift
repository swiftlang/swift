// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : Comparable
enum Simple: Comparable {
  // CHECK:       @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:     switch lhs {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_lhs  =  0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_lhs  =  1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_rhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_rhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_lhs < index_rhs
  // CHECK-NEXT:  }
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY: 
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_lhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_lhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // 
  //   switch rhs {
  //   case .a:
  //     index_rhs  =  0
  //   case .b:
  //     index_rhs  =  1
  //   }
  //   return index_lhs == index_rhs
  // }


  // CHECK:        case a
  case a
  // CHECK:        case b
  case b
}

// CHECK-LABEL: internal enum HasAssociatedValues : Comparable
enum HasAssociatedValues: Comparable {
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    switch (lhs, rhs) {
  // CHECK-NEXT:    case (.a(let l0), .a(let r0)):
  // CHECK-NEXT:      guard l0 == r0 else {
  // CHECK-NEXT:        return false
  // CHECK-NEXT:      }
  // CHECK-NEXT:      return true
  // CHECK-NEXT:    case (.b(let l0), .b(let r0)):
  // CHECK-NEXT:      guard l0 == r0 else {
  // CHECK-NEXT:        return false
  // CHECK-NEXT:      }
  // CHECK-NEXT:      return true
  // CHECK-NEXT:    case (.c, .c):
  // CHECK-NEXT:      return true
  // CHECK-NEXT:    default:
  // CHECK-NEXT:      return false
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
  // CHECK:       @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_lhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_lhs  =  1
  // CHECK-NEXT:    case .c:
  // CHECK-NEXT:      index_lhs  =  2
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_rhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_rhs  =  1
  // CHECK-NEXT:    case .c:
  // CHECK-NEXT:      index_rhs  =  2
  // CHECK-NEXT:    }
  // CHECK-NEXT:    if index_lhs != index_rhs {
  // CHECK-NEXT:      return index_lhs < index_rhs
  // CHECK-NEXT:    }
  // CHECK-NEXT:    switch (lhs, rhs) {
  // CHECK-NEXT:    case (.a(let l0), .a(let r0)):
  // CHECK-NEXT:      guard l0 == r0 else {
  // CHECK-NEXT:        return l0 < r0
  // CHECK-NEXT:      }
  // CHECK-NEXT:      return false
  // CHECK-NEXT:    case (.b(let l0), .b(let r0)):
  // CHECK-NEXT:      guard l0 == r0 else {
  // CHECK-NEXT:        return l0 < r0
  // CHECK-NEXT:      }
  // CHECK-NEXT:      return false
  // CHECK-NEXT:    case (.c, .c):
  // CHECK-NEXT:      return false
  // CHECK-NEXT:    default:
  // CHECK-NEXT:      Swift::fatalError("Unavailable code reached")
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK:        case a(Int)
  case a(Int)
  // CHECK:        case b(String)
  case b(String)
  // CHECK:        case c
  case c
}

// CHECK-LABEL: internal enum UnavailableEnum : Comparable
@available(*, unavailable)
enum UnavailableEnum: Comparable {
  // CHECK:       @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_lhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_lhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_rhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_rhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_lhs < index_rhs
  // CHECK-NEXT:  }
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_lhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_lhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_rhs  =  0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      index_rhs  =  1
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_lhs == index_rhs
  // CHECK-NEXT:  }
 
  // CHECK:        case a
  case a
  // CHECK:        case b
  case b
}

// internal enum Empty : Comparable {
enum Empty: Comparable {
  //CHECK:      @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  //CHECK-NEXT: }
  //CHECK:       @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  //CHECK-NEXT: }
}
