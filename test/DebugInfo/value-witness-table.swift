// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK

// Test that we don't generate debug info for hidden global variables.

// CHECK: @_T04main2E1OWV ={{.*}} constant
// CHECK-NOT: !DIGlobalVariable
public enum E1 { case E }
public func f() -> E1 { return .E }
