// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum Simple: Int {
  case a
  case b
}

// CHECK:       init?(rawValue: Int) {
// CHECK-NEXT:    switch rawValue {
// CHECK-NEXT:    case 0:
// CHECK-EMPTY:
// CHECK-NEXT:      self = .a
// CHECK-NEXT:    case 1:
// CHECK-EMPTY:
// CHECK-NEXT:      self = .b
// CHECK-NEXT:    default:
// CHECK-NEXT:      return nil
// CHECK-NEXT:    }
// CHECK-NEXT:  }

// CHECK:      nonisolated var rawValue: Int {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .a:
// CHECK-NEXT:       return 0
// CHECK-NEXT:   case .b:
// CHECK-NEXT:       return 1
// CHECK-NEXT:   }
// CHECK-NEXT: }

enum Strings: String {
  case a
  case b = "bee"
}

// CHECK:      init?(rawValue: String) {
// CHECK-NEXT:   switch rawValue {
// CHECK-NEXT:   case "a":
// CHECK-EMPTY: 
// CHECK-NEXT:   self = .a
// CHECK-NEXT:   case "bee":
// CHECK-EMPTY: 
// CHECK-NEXT:     self = .b
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:        nonisolated var rawValue: String {
// CHECK-NEXT:    switch self {
// CHECK-NEXT:    case .a:
// CHECK-NEXT:        return "a"
// CHECK-NEXT:    case .b:
// CHECK-NEXT:        return "bee"
// CHECK-NEXT:    }
// CHECK-NEXT:  }

enum EscapedNames: String {
  case `default`
  case `foo bar`
}

// CHECK:        init?(rawValue: String) {
// CHECK-NEXT:    switch rawValue {
// CHECK-NEXT:    case "default":
// CHECK-EMPTY:
// CHECK-NEXT:    self = .`default`
// CHECK-NEXT:    case "foo bar":
// CHECK-EMPTY:
// CHECK-NEXT:      self = .`foo bar`
// CHECK-NEXT:    default:
// CHECK-NEXT:      return nil
// CHECK-NEXT:    }
// CHECK-NEXT:  }

// CHECK:      nonisolated var rawValue: String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .`default`:
// CHECK-NEXT:       return "default"
// CHECK-NEXT:   case .`foo bar`:
// CHECK-NEXT:       return "foo bar"
// CHECK-NEXT:   }
// CHECK-NEXT: }
