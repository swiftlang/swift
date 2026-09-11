// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s

// CHECK-LABEL: internal enum Simple : Comparable
enum Simple: Comparable {
  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: Simple, _ b: Simple) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a == index_b
  // CHECK-NEXT:   }
 
  // CHECK:        @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ a: Simple, _ b: Simple) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a < index_b
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum HasAssociatedValues : Comparable
enum HasAssociatedValues: Comparable {
  // CHECK:        case a(Int)
  case a(Int)
  // CHECK:        case b(String)
  case b(String)
  // CHECK:        case c
  case c

  // CHECK:        @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: HasAssociatedValues, _ b: HasAssociatedValues) -> Bool {
  // CHECK-NEXT:     switch (a, b) {
  // CHECK-NEXT:     case (.a(let l0), .a(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.b(let l0), .b(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.c, .c):
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ a: HasAssociatedValues, _ b: HasAssociatedValues) -> Bool {
  // CHECK-NEXT:     switch (a, b) {
  // CHECK-NEXT:     case (.a(let l0), .a(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return l0 < r0
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     case (.b(let l0), .b(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return l0 < r0
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     case (.c, .c):
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       var index_a: Int
  // CHECK-NEXT:       switch a {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         index_a = 0
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         index_a = 1
  // CHECK-NEXT:       case .c:
  // CHECK-NEXT:         index_a = 2
  // CHECK-NEXT:       }
  // CHECK-NEXT:       var index_b: Int
  // CHECK-NEXT:       switch b {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         index_b = 0
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         index_b = 1
  // CHECK-NEXT:       case .c:
  // CHECK-NEXT:         index_b = 2
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return index_a < index_b
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum UnavailableEnum : Comparable
@available(*, unavailable)
enum UnavailableEnum: Comparable {
  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: UnavailableEnum, _ b: UnavailableEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a == index_b
  // CHECK-NEXT:   }

  // CHECK:        @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ a: UnavailableEnum, _ b: UnavailableEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a < index_b
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum Empty : Comparable
enum Empty: Comparable {
  // CHECK:      @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: Empty, _ b: Empty) -> Bool {
  // CHECK-NEXT:   switch (a, b) {
  // CHECK-NEXT:   }
  // CHECK-NEXT: }
  // CHECK:      @_implements(Comparable, <(_:_:)) internal static func __derived_enum_less_than(_ a: Empty, _ b: Empty) -> Bool {
  // CHECK-NEXT: }
}
