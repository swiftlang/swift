// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-feature NonexhaustiveAttribute -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-feature NonexhaustiveAttribute -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_NonexhaustiveAttribute

// CHECK: #if compiler(>=5.3) && $NonexhaustiveAttribute
// CHECK-NEXT: @nonexhaustive public enum E {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public enum E {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@nonexhaustive
public enum E {
}

// CHECK: #if compiler(>=5.3) && $NonexhaustiveAttribute
// CHECK-NEXT: @nonexhaustive(warn) public enum F {
// CHECK: #else
// CHECK-NEXT: public enum F {
// CHECK: #endif
@nonexhaustive(warn)
public enum F {
  case a
}
