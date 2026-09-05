// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s

// CHECK-LABEL: internal enum Simple : CodingKey
enum Simple: CodingKey {
  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = Simple.a
  // CHECK-NEXT:     case "b":
  // CHECK-NEXT:       self = Simple.b
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
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
}

// CHECK-LABEL: internal enum StringRaw : String, CodingKey
enum StringRaw: String, CodingKey {
  // CHECK:        case alpha
  case alpha = "a"
  // CHECK:        case beta
  case beta = "b"

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum IntRaw : Int, CodingKey
enum IntRaw: Int, CodingKey {
  // CHECK:        case one
  case one = 1
  // CHECK:        case two
  case two = 2

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "one":
  // CHECK-NEXT:       self = IntRaw.one
  // CHECK-NEXT:     case "two":
  // CHECK-NEXT:       self = IntRaw.two
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     self.init(rawValue: intValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
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
}

// CHECK-LABEL: internal enum Empty : CodingKey
enum Empty: CodingKey {
  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal init?(intValue: Int) {
  // CHECK-NEXT:     return nil
  // CHECK-NEXT:   }

  // CHECK:        internal var intValue: Int? {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return ""
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum Escaped : CodingKey
enum Escaped: CodingKey {
  // CHECK:        case `default`
  case `default`
  // CHECK:        case `init`
  case `init`
  // CHECK:        case `self`
  case `self`

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "default":
  // CHECK-NEXT:       self = Escaped.default
  // CHECK-NEXT:     case "init":
  // CHECK-NEXT:       self = Escaped.init
  // CHECK-NEXT:     case "self":
  // CHECK-NEXT:       self = Escaped.self
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
}

// CHECK-LABEL: internal enum EscapedWithStringRaw : String, CodingKey
enum EscapedWithStringRaw: String, CodingKey {
  // CHECK:        case `default`
  case `default` = "def"
  // CHECK:        case `class`
  case `class` = "cls"

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     self.init(rawValue: stringValue)
  // CHECK-NEXT:   }

  // CHECK:        internal var stringValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return self.rawValue
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL: internal enum RawIdentifiers : CodingKey
enum RawIdentifiers: CodingKey {
  // CHECK:        case `hello world`
  case `hello world`
  // CHECK:        case `if`
  case `if`

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "hello world":
  // CHECK-NEXT:       self = RawIdentifiers.hello world
  // CHECK-NEXT:     case "if":
  // CHECK-NEXT:       self = RawIdentifiers.if
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
}

// CHECK-LABEL: internal enum HasUnavailableElement : CodingKey
enum HasUnavailableElement: CodingKey {
  // CHECK:        case a
  case a
  // CHECK:        @available(*, unavailable)
  // CHECK-NEXT:   case b
  @available(*, unavailable)
  case b

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = HasUnavailableElement.a
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
}

// CHECK-LABEL: internal enum UnavailableEnum : CodingKey
@available(*, unavailable)
enum UnavailableEnum: CodingKey {
  // CHECK:        case a
  case a
  // CHECK:        case b
  case b

  // CHECK:        internal init?(stringValue: String) {
  // CHECK-NEXT:     switch stringValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = UnavailableEnum.a
  // CHECK-NEXT:     case "b":
  // CHECK-NEXT:       self = UnavailableEnum.b
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
}
