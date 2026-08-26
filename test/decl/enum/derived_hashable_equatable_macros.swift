// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : Hashable
enum Simple: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:     switch lhs {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_lhs = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_lhs = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:     switch rhs {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_rhs = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_rhs = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_lhs == index_rhs
  // CHECK-NEXT:   }

  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     var discriminator: Int
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       discriminator = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       discriminator = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     hasher.combine(discriminator)
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum HasAssociatedValues : Hashable
enum HasAssociatedValues: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     switch (lhs, rhs) {
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

  // CHECK:        case a(Int)
  case a(Int)
  // CHECK:        case b(String)
  case b(String)
  // CHECK:        case c
  case c

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .a(let a0):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .b(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .c:
  // CHECK-NEXT:       hasher.combine(2)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum HasUnavailableElement : Hashable
enum HasUnavailableElement: Hashable {
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_lhs = 0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      fatalError({{.*}})
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      index_rhs = 0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      fatalError({{.*}})  
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_lhs == index_rhs
  // CHECK-NEXT:  }

  // CHECK:       case a
  case a
  // CHECK:       @available(*, unavailable)
  // CHECK-NEXT:  case b
  @available(*, unavailable)
  case b

  // CHECK:       internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:    var discriminator: Int
  // CHECK-NEXT:    switch self {
  // CHECK-NEXT:    case .a:
  // CHECK-NEXT:      discriminator = 0
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}()
  // CHECK-NEXT:    }
  // CHECK-NEXT:    hasher.combine(discriminator)
  // CHECK-NEXT:  }

  // CHECK:       internal var hashValue: Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return _hashValue(for: self)
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
}

// CHECK-LABEL: internal enum HasAssociatedValuesAndUnavailableElement : Hashable
enum HasAssociatedValuesAndUnavailableElement: Hashable {
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    switch (lhs, rhs) {
  // CHECK-NEXT:    case (.a(let l0), .a(let r0)):
  // CHECK-NEXT:      guard l0 == r0 else {
  // CHECK-NEXT:        return false
  // CHECK-NEXT:      }
  // CHECK-NEXT:      return true
  // CHECK-NEXT:    case (.b, .b):
  // CHECK-NEXT:      fatalError({{.*}})
  // CHECK-NEXT:    default:
  // CHECK-NEXT:      return false
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK:        case a(Int)
  case a(Int)
  // CHECK:       @available(*, unavailable)
  // CHECK-NEXT:  case b(String)
  @available(*, unavailable)
  case b(String)

  // CHECK:       internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:    switch self {
  // CHECK-NEXT:    case .a(let a0):
  // CHECK-NEXT:      hasher.combine(0)
  // CHECK-NEXT:      hasher.combine(a0)
  // CHECK-NEXT:    case .b:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}()
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK:       internal var hashValue: Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return _hashValue(for: self)
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
}

// CHECK-LABEL: internal enum UnavailableEnum : Hashable
@available(*, unavailable)
enum UnavailableEnum: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:     switch lhs {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_lhs = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_lhs = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:     switch rhs {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       index_rhs = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       index_rhs = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_lhs == index_rhs
  // CHECK-NEXT:   }

  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     var discriminator: Int
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .a:
  // CHECK-NEXT:       discriminator = 0
  // CHECK-NEXT:     case .b:
  // CHECK-NEXT:       discriminator = 1
  // CHECK-NEXT:     }
  // CHECK-NEXT:     hasher.combine(discriminator)
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

}

// CHECK-LABEL: internal enum MultipleCasesInLine : Hashable
enum MultipleCasesInLine: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     switch (lhs, rhs) {
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
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a(Int), b(String)
  case a(Int), b(String)

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .a(let a0):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .b(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum MultipleAssociatedValues : Hashable
enum MultipleAssociatedValues: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     switch (lhs, rhs) {
  // CHECK-NEXT:     case (.a(let l0, let l1), .a(let r0, let r1)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard l1 == r1 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.b(let l0, let l1, let l2), .b(let r0, let r1, let r2)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard l1 == r1 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard l2 == r2 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a(Int, String)
  case a(Int, String)
  // CHECK:        case b(Int, String, Bool)
  case b(Int, String, Bool)

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .a(let a0, let a1):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:       hasher.combine(a1)
  // CHECK-NEXT:     case .b(let a0, let a1, let a2):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:       hasher.combine(a1)
  // CHECK-NEXT:       hasher.combine(a2)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum WithArgumentLabels : Hashable
enum WithArgumentLabels: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     switch (lhs, rhs) {
  // CHECK-NEXT:     case (.a(x: let l0, y: let l1), .a(x: let r0, y: let r1)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard l1 == r1 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.b(value: let l0), .b(value: let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a(x: Int, y: String)
  case a(x: Int, y: String)
  // CHECK:        case b(value: Bool)
  case b(value: Bool)
  
  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum WithRawIdentifiers : Hashable
enum WithRawIdentifiers: Hashable {
  // CHECK:        @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:     switch (lhs, rhs) {
  // CHECK-NEXT:     case (.foo bar, .foo bar):
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.default(let l0), .default(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.a(foo bar: let l0), .a(foo bar: let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case `foo bar`
  case `foo bar`
  // CHECK:        case `default`(Int)
  case `default`(Int)
  // CHECK:        case a(`foo bar`: String)
  case a(`foo bar`: String)

  // CHECK:        internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .foo bar:
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:     case .default(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .a(let a0):
  // CHECK-NEXT:       hasher.combine(2)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var hashValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return _hashValue(for: self)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}
