// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s

// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -emit-sil -o /dev/null %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -emit-sil -o /dev/null %s -enable-library-evolution

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum Simple : Int {
enum Simple: Int {
  // CHECK-NEXT:   nonisolated internal var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  // CHECK-NEXT:   internal init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     case 1:
  // CHECK-NEXT:       self = .b
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  // CHECK-NEXT:   case a
  case a
  // CHECK-NEXT:   case b
  case b
  // CHECK-NEXT:   internal typealias RawValue = Int
}

// CHECK-LABEL: internal enum Strings : String {
enum Strings: String {
  // CHECK-NEXT:   nonisolated internal var rawValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return "a"
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return "bee"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  // CHECK-NEXT:   internal init?(rawValue: String) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case "a":
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     case "bee":
  // CHECK-NEXT:       self = .b
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  case a
  case b = "bee"
}

// CHECK-LABEL: internal enum EscapedNames : String {
enum EscapedNames: String {
  // CHECK-NEXT:   nonisolated internal var rawValue: String {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .default:
  // CHECK-NEXT:         return "default"
  // CHECK-NEXT:       case .foo bar:
  // CHECK-NEXT:         return "foo bar"
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  // CHECK-NEXT:   internal init?(rawValue: String) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case "default":
  // CHECK-NEXT:       self = .default
  // CHECK-NEXT:     case "foo bar":
  // CHECK-NEXT:       self = .foo bar
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  case `default`
  case `foo bar`
}

// CHECK-LABEL: internal enum ExplicitRawValue : Int, RawRepresentable {
enum ExplicitRawValue: Int, RawRepresentable {
  // CHECK-NEXT:   case a
  case a = 0

  // CHECK-NEXT:   internal var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       return 42
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  var rawValue: Int { return 42 }

  // CHECK:        internal init?(rawValue: Int) {
  // CHECK-NEXT:     self = ExplicitRawValue.a
  init?(rawValue: Int) { self = .a }
}
