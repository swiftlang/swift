// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test -enable-experimental-feature BitwiseCopyable
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test


@frozen
@_moveOnly
public struct S_Implicit_Noncopyable {}

// CHECK-NOT: extension Test.S_Implicit_Noncopyable : Swift._BitwiseCopyable {}
