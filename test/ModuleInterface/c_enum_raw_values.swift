// RUN: %empty-directory(%t)

// @c enums, like @objc enums, keep their raw values in the module interface.

// RUN: %target-swift-frontend -typecheck -swift-version 5 -enable-library-evolution -module-name CEnums -emit-module-interface-path %t/CEnums.swiftinterface %s
// RUN: %FileCheck %s < %t/CEnums.swiftinterface

// CHECK: @c
// CHECK-NEXT: public enum CEnum : {{(Swift::)?}}Int32 {
// CHECK-NEXT: case a = 1, b, c = 10
@c public enum CEnum: Int32 { case a = 1, b, c = 10 }

// Without @c or @objc, the raw values are omitted.
// CHECK: public enum SwiftEnum : {{(Swift::)?}}Int32 {
// CHECK-NEXT: case a, b, c
public enum SwiftEnum: Int32 { case a = 1, b, c = 10 }
