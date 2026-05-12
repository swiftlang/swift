// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// Verify the DW_OP_deref count when shadow-copying address-only values with
// usesMoveableValueDebugInfo. When a shadow copy is emitted and
// usesMoveableValueDebugInfo is set, IndirectValue adds an extra DW_OP_deref.
//
// Case 1 (local variable): AllocStackHoisting hoists the alloc_stack to a
//   dynamic alloca and creates a debug_value WITHOUT op_deref.
//   Derefs: IndirectValue(1) + DbgValueDeref(1) = 2
//
// Case 2 (inout parameter): The debug_value has op_deref.
//   Derefs: op_deref(1) + IndirectValue(1) + DbgValueDeref(1) = 3

public protocol P {
    mutating func use()
}

// Case 1: local address-only variable (hoisted alloc_stack, no op_deref).
// The alloc_stack for `k` gets [moveable_value_debuginfo] and is hoisted
// to a dynamic alloca. The shadow copy adds an op_deref.
//
// CHECK-LABEL: define {{.*}} @"$s{{.*}}9testLocal{{.*}}"(
// CHECK: #dbg_value(ptr %k.debug, ![[LOCAL_K:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[LOCAL_LOC:[0-9]+]]
// CHECK: #dbg_value(ptr undef, ![[LOCAL_K]], !DIExpression(DW_OP_deref), ![[LOCAL_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func testLocal<T: P>(_ x: T) {
    var k = x
    k.use()
    let _ = consume k
}

// Case 2: inout address-only parameter (has op_deref).
// The debug_value's operand is the indirect parameter (not an
// AllocStackInst), so op_deref is prepended. The shadow copy adds an op_deref.
//
// CHECK-LABEL: define {{.*}} @"$s{{.*}}12testInoutArg{{.*}}"(
// CHECK: #dbg_value(ptr %k.debug, ![[ARG_K:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ARG_LOC:[0-9]+]]
// CHECK: #dbg_value(ptr undef, ![[ARG_K]], !DIExpression(DW_OP_deref), ![[ARG_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func testInoutArg<T: P>(_ k: inout T) {
    k.use()
    let m = consume k
    k = m
}
