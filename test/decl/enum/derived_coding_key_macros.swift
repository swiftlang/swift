// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : CodingKey
enum Simple: CodingKey {

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }
  
  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     case "b":
  // CHECK-NEXT:       self = .b
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return "a"
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return "b"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a
  case a
  // CHECK:        case b
  case b
}

// A String raw type means both string witnesses forward to the raw value.

// CHECK-LABEL: internal enum StringRaw : String, CodingKey
enum StringRaw: String, CodingKey {
  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }


  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NEXT:   }


  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case alpha
  case alpha = "a"
  // CHECK:        case beta
  case beta = "b"
}

// An Int raw type means the int witnesses forward to the raw value, while the
// string witnesses are still derived from the element names.

// CHECK-LABEL: internal enum IntRaw : Int, CodingKey
enum IntRaw: Int, CodingKey {

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     self.init(rawValue: intValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "one":
  // CHECK-NEXT:       self = .one
  // CHECK-NEXT:     case "two":
  // CHECK-NEXT:       self = .two
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .one:
  // CHECK-NEXT:         return "one"
  // CHECK-NEXT:       case .two:
  // CHECK-NEXT:         return "two"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case one
  case one = 1
  // CHECK:        case two
  case two = 2
}

// CHECK-LABEL: internal enum Empty : CodingKey
enum Empty: CodingKey {

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return ""
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// The string value of an escaped element is its name without the backticks.

// CHECK-LABEL: internal enum Escaped : CodingKey
enum Escaped: CodingKey {

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "default":
  // CHECK-NEXT:       self = .default
  // CHECK-NEXT:     case "init":
  // CHECK-NEXT:       self = .init
  // CHECK-NEXT:     case "self":
  // CHECK-NEXT:       self = .self
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .default:
  // CHECK-NEXT:         return "default"
  // CHECK-NEXT:       case .init:
  // CHECK-NEXT:         return "init"
  // CHECK-NEXT:       case .self:
  // CHECK-NEXT:         return "self"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case `default`
  case `default`
  // CHECK:        case `init`
  case `init`
  // CHECK:        case `self`
  case `self`
}

// CHECK-LABEL: internal enum EscapedWithStringRaw : String, CodingKey
enum EscapedWithStringRaw: String, CodingKey {


  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case `default`
  case `default` = "def"
  // CHECK:        case `class`
  case `class` = "cls"
}

// CHECK-LABEL: internal enum RawIdentifiers : CodingKey
enum RawIdentifiers: CodingKey {

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "hello world":
  // CHECK-NEXT:       self = .hello world
  // CHECK-NEXT:     case "if":
  // CHECK-NEXT:       self = .if
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .hello world:
  // CHECK-NEXT:         return "hello world"
  // CHECK-NEXT:       case .if:
  // CHECK-NEXT:         return "if"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case `hello world`
  case `hello world`
  // CHECK:        case `if`
  case `if`

}

// Unavailable elements can't be produced at runtime, so `init?(stringValue:)`
// never matches them. `stringValue` still has to handle them, since an
// unavailable element may still be formed in unavailable code.

// CHECK-LABEL: internal enum HasUnavailableElement : CodingKey
enum HasUnavailableElement: CodingKey {

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return "a"
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return "b"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a
  case a
  // CHECK:        @available(*, unavailable)
  // CHECK-NEXT:   case b
  @available(*, unavailable)
  case b

}

// An unavailable enum has available elements, so nothing is skipped.

// CHECK-LABEL: internal enum UnavailableEnum : CodingKey
@available(*, unavailable)
enum UnavailableEnum: CodingKey {


  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     case "b":
  // CHECK-NEXT:       self = .b
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return "a"
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return "b"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        case a
  case a
  // CHECK:        case b
  case b
}
