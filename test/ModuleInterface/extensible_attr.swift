// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -enable-experimental-feature ExtensibleAttribute -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -enable-experimental-feature ExtensibleAttribute -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_feature_ExtensibleAttribute

// CHECK: #if compiler(>=5.3) && $ExtensibleAttribute
// CHECK-NEXT: @extensible public enum E {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public enum E {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@extensible
public enum E {
}

// CHECK: #if compiler(>=5.3) && $ExtensibleAttribute
// CHECK-NEXT: @preEnumExtensibility @extensible public enum F {
// CHECK: #else
// CHECK-NEXT: public enum F {
// CHECK: #endif
@preEnumExtensibility
@extensible
public enum F {
  case a
}
