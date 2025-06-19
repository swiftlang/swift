// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test

// This test fails if we suppress the @escaping attribute recursively
// when printing enum element parameter lists instead of only suppressing
// it when printing the immediate type of the enum payload.

public enum A {
  case function(_: (@escaping () -> Void) -> Void)
}

// CHECK-LABEL: public enum A {
// CHECK-NEXT:    case function((@escaping () -> Swift.Void) -> Swift.Void)
// CHECK-NEXT:  }
