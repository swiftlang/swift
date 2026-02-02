// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -parse-stdlib -module-name Swift
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -parse-stdlib -module-name Swift

// CHECK:      public protocol BitwiseCopyable {
// CHECK-NEXT: }

// CHECK-NEXT: public typealias _BitwiseCopyable = Swift.BitwiseCopyable
public protocol BitwiseCopyable {}
public typealias _BitwiseCopyable = BitwiseCopyable

