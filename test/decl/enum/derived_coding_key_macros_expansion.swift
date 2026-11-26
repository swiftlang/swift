// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: CodingKey {
  case a
  case b
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:     return "a"
// CHECK-NEXT:   case .b:
// CHECK-NEXT:     return "b"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "a":
// CHECK-NEXT:     self = .a
// CHECK-NEXT:   case "b":
// CHECK-NEXT:     self = .b
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      var intValue: Swift::Int? {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

// CHECK:      init?(intValue: Swift::Int) {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

enum StringRaw: String, CodingKey {
  case alpha = "a"
  case beta = "b"
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   return self.rawValue
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   self.init(rawValue: stringValue)
// CHECK-NEXT: }

// CHECK:      var intValue: Swift::Int? {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

// CHECK:      init?(intValue: Swift::Int) {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

enum IntRaw: Int, CodingKey {
  case one = 1
  case two = 2
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .one:
// CHECK-NEXT:     return "one"
// CHECK-NEXT:   case .two:
// CHECK-NEXT:     return "two"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "one":
// CHECK-NEXT:     self = .one
// CHECK-NEXT:   case "two":
// CHECK-NEXT:     self = .two
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      var intValue: Swift::Int? {
// CHECK-NEXT:   return self.rawValue
// CHECK-NEXT: }

// CHECK:      init?(intValue: Swift::Int) {
// CHECK-NEXT:   self.init(rawValue: intValue)
// CHECK-NEXT: }

enum Empty: CodingKey {}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   return ""
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-EMPTY:
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      var intValue: Swift::Int? {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

// CHECK:      init?(intValue: Swift::Int) {
// CHECK-NEXT:   return nil
// CHECK-NEXT: }

enum Escaped: CodingKey {
  case `default`
  case `init`
  case `self`
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .`default`:
// CHECK-NEXT:     return "default"
// CHECK-NEXT:   case .`init`:
// CHECK-NEXT:     return "init"
// CHECK-NEXT:   case .`self`:
// CHECK-NEXT:     return "self"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "default":
// CHECK-NEXT:     self = .`default`
// CHECK-NEXT:   case "init":
// CHECK-NEXT:     self = .`init`
// CHECK-NEXT:   case "self":
// CHECK-NEXT:     self = .`self`
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

enum EscapedWithStringRaw: String, CodingKey {
  case `default` = "def"
  case `class` = "cls"
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   return self.rawValue
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   self.init(rawValue: stringValue)
// CHECK-NEXT: }

enum RawIdentifiers: CodingKey {
  case `hello world`
  case `if`
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .`hello world`:
// CHECK-NEXT:     return "hello world"
// CHECK-NEXT:   case .`if`:
// CHECK-NEXT:     return "if"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "hello world":
// CHECK-NEXT:     self = .`hello world`
// CHECK-NEXT:   case "if":
// CHECK-NEXT:     self = .`if`
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

enum HasUnavailableElement: CodingKey {
  case a
  @available(*, unavailable)
  case b
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:     return "a"
// CHECK-NEXT:   case .b:
// CHECK-NEXT:     return "b"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "a":
// CHECK-NEXT:     self = .a
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

@available(*, unavailable)
enum UnavailableEnum: CodingKey {
  case a
  case b
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:     return "a"
// CHECK-NEXT:   case .b:
// CHECK-NEXT:     return "b"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "a":
// CHECK-NEXT:     self = .a
// CHECK-NEXT:   case "b":
// CHECK-NEXT:     self = .b
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }
