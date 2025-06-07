// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test


@frozen
public struct S_Implicit_Noncopyable: ~Copyable {}

// CHECK-NOT: extension Test.S_Implicit_Noncopyable : Swift.BitwiseCopyable {}

// CHECK:      public protocol BitwiseCopyable {
// CHECK-NEXT: }
// CHECK-NEXT: public typealias _BitwiseCopyable = Test.BitwiseCopyable
public protocol BitwiseCopyable {}
public typealias _BitwiseCopyable = BitwiseCopyable
