// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-MACRO

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : CodingKey
enum Simple: CodingKey {
  // CHECK-NORMAL:        case a
  // CHECK-NORMAL:        case b

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "a":
  // CHECK-NORMAL-NEXT:       self = Simple.a
  // CHECK-NORMAL-NEXT:     case "b":
  // CHECK-NORMAL-NEXT:       self = Simple.b
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "a":
  // CHECK-MACRO-NEXT:       self = .a
  // CHECK-MACRO-NEXT:     case "b":
  // CHECK-MACRO-NEXT:       self = .b
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

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

  // CHECK-MACRO:        case a
  case a
  // CHECK-MACRO:        case b
  case b
}

// A String raw type means both string witnesses forward to the raw value.

// CHECK-LABEL: internal enum StringRaw : String, CodingKey
enum StringRaw: String, CodingKey {
  // CHECK-NORMAL:        case alpha
  // CHECK-NORMAL:        case beta

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NORMAL-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     self.init(rawValue: stringValue)
  // CHECK-MACRO-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        case alpha
  case alpha = "a"
  // CHECK-MACRO:        case beta
  case beta = "b"
}

// An Int raw type means the int witnesses forward to the raw value, while the
// string witnesses are still derived from the element names.

// CHECK-LABEL: internal enum IntRaw : Int, CodingKey
enum IntRaw: Int, CodingKey {
  // CHECK-NORMAL:        case one
  // CHECK-NORMAL:        case two

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "one":
  // CHECK-NORMAL-NEXT:       self = IntRaw.one
  // CHECK-NORMAL-NEXT:     case "two":
  // CHECK-NORMAL-NEXT:       self = IntRaw.two
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     self.init(rawValue: intValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "one":
  // CHECK-MACRO-NEXT:       self = .one
  // CHECK-MACRO-NEXT:     case "two":
  // CHECK-MACRO-NEXT:       self = .two
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

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

  // CHECK-MACRO:        case one
  case one = 1
  // CHECK-MACRO:        case two
  case two = 2
}

// CHECK-LABEL: internal enum Empty : CodingKey
enum Empty: CodingKey {
  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     return nil
  // CHECK-NORMAL-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return ""
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// The string value of an escaped element is its name without the backticks.

// CHECK-LABEL: internal enum Escaped : CodingKey
enum Escaped: CodingKey {
  // CHECK-NORMAL:        case `default`
  // CHECK-NORMAL:        case `init`
  // CHECK-NORMAL:        case `self`

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "default":
  // CHECK-NORMAL-NEXT:       self = Escaped.default
  // CHECK-NORMAL-NEXT:     case "init":
  // CHECK-NORMAL-NEXT:       self = Escaped.init
  // CHECK-NORMAL-NEXT:     case "self":
  // CHECK-NORMAL-NEXT:       self = Escaped.self
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "default":
  // CHECK-MACRO-NEXT:       self = .default
  // CHECK-MACRO-NEXT:     case "init":
  // CHECK-MACRO-NEXT:       self = .init
  // CHECK-MACRO-NEXT:     case "self":
  // CHECK-MACRO-NEXT:       self = .self
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

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

  // CHECK-MACRO:        case `default`
  case `default`
  // CHECK-MACRO:        case `init`
  case `init`
  // CHECK-MACRO:        case `self`
  case `self`
}

// CHECK-LABEL: internal enum EscapedWithStringRaw : String, CodingKey
enum EscapedWithStringRaw: String, CodingKey {
  // CHECK-NORMAL:        case `default`
  // CHECK-NORMAL:        case `class`

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-MACRO:        case `default`
  case `default` = "def"
  // CHECK-MACRO:        case `class`
  case `class` = "cls"
}

// CHECK-LABEL: internal enum RawIdentifiers : CodingKey
enum RawIdentifiers: CodingKey {
  // CHECK-NORMAL:        case `hello world`
  // CHECK-NORMAL:        case `if`

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "hello world":
  // CHECK-NORMAL-NEXT:       self = RawIdentifiers.hello world
  // CHECK-NORMAL-NEXT:     case "if":
  // CHECK-NORMAL-NEXT:       self = RawIdentifiers.if
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // FIXME: Because of an ASTPrinter bug, raw identifiers aren't escaped 
  // e.g. "case .`hello world`" is just "case .hello world" here, ditto for `if`.

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "hello world":
  // CHECK-MACRO-NEXT:       self = .hello world
  // CHECK-MACRO-NEXT:     case "if":
  // CHECK-MACRO-NEXT:       self = .if
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

  // FIXME: Because of an ASTPrinter bug, raw identifiers aren't escaped 
  // e.g. "case .`hello world`" is just "case .hello world" here, ditto for `if`.
  
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

  // CHECK-MACRO:        case `hello world`
  case `hello world`
  // CHECK-MACRO:        case `if`
  case `if`
}

// Unavailable elements can't be produced at runtime, so `init?(stringValue:)`
// never matches them. `stringValue` still has to handle them, since an
// unavailable element may still be formed in unavailable code.

// CHECK-LABEL: internal enum HasUnavailableElement : CodingKey
enum HasUnavailableElement: CodingKey {
  // CHECK-NORMAL:        case a
  // CHECK-NORMAL:        @available(*, unavailable)
  // CHECK-NORMAL-NEXT:   case b

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "a":
  // CHECK-NORMAL-NEXT:       self = HasUnavailableElement.a
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "a":
  // CHECK-MACRO-NEXT:       self = .a
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

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

  // CHECK-MACRO:        case a
  case a
  // CHECK-MACRO:        @available(*, unavailable)
  // CHECK-MACRO-NEXT:   case b
  @available(*, unavailable)
  case b
}

// An unavailable enum has available elements, so nothing is skipped.

// CHECK-LABEL: internal enum UnavailableEnum : CodingKey
@available(*, unavailable)
enum UnavailableEnum: CodingKey {
  // CHECK-NORMAL:        case a
  // CHECK-NORMAL:        case b

  // CHECK-NORMAL:        internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:     switch stringValue {
  // CHECK-NORMAL-NEXT:     case "a":
  // CHECK-NORMAL-NEXT:       self = UnavailableEnum.a
  // CHECK-NORMAL-NEXT:     case "b":
  // CHECK-NORMAL-NEXT:       self = UnavailableEnum.b
  // CHECK-NORMAL-NEXT:     default:
  // CHECK-NORMAL-NEXT:       return nil
  // CHECK-NORMAL-NEXT:     }
  // CHECK-NORMAL-NEXT:   }

  // CHECK-MACRO:        internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:     switch stringValue {
  // CHECK-MACRO-NEXT:     case "a":
  // CHECK-MACRO-NEXT:       self = .a
  // CHECK-MACRO-NEXT:     case "b":
  // CHECK-MACRO-NEXT:       self = .b
  // CHECK-MACRO-NEXT:     default:
  // CHECK-MACRO-NEXT:       return nil
  // CHECK-MACRO-NEXT:     }
  // CHECK-MACRO-NEXT:   }

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

  // CHECK-MACRO:        case a
  case a
  // CHECK-MACRO:        case b
  case b
}
