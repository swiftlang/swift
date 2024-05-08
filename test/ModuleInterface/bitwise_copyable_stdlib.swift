// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -parse-stdlib -module-name Swift
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -parse-stdlib -module-name Swift

// CHECK:      #if compiler(>=5.3) && $BitwiseCopyable2
// CHECK-NEXT: public protocol BitwiseCopyable {
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public protocol _BitwiseCopyable {
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK:      #if compiler(>=5.3) && $BitwiseCopyable2
// CHECK-NEXT: public typealias _BitwiseCopyable = Swift.BitwiseCopyable
// CHECK-NEXT: #else
// CHECK-NEXT: public typealias BitwiseCopyable = Swift._BitwiseCopyable
// CHECK-NEXT: #endif
public protocol BitwiseCopyable {}
public typealias _BitwiseCopyable = BitwiseCopyable

