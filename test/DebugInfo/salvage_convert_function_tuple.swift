// RUN: %target-swift-frontend %s -O -emit-sil -g | %FileCheck %s

// Fixed crasher: iterating over a tuple array containing a
// () throws -> Any closure caused a verifier failure because
// tryEliminateOnlyOwnershipUsedForwardingInst RAUW'ed instead of salvaging.

// The ignored tuple element is killed, which leaves the closure half of the element.
// CHECK: debug_value [[ELEMENT:%[0-9]+]], let, name "$element", transform {
// CHECK-NEXT: // %0
// CHECK-NEXT: bb0(%0 :
// CHECK-NEXT:   %1 = convert_function %0 to $@callee_guaranteed () -> (@out Any, @error any Error)
// CHECK-NEXT:   %2 = tuple (undef : $Int, %1)
// CHECK-NEXT:   return %2
// CHECK-NEXT: }

func test() {
    let a: [(Int, () throws -> Any)] = [(0, { 1 })]
    for (_, f) in a {
        _ = try! f()
    }
}

test()
