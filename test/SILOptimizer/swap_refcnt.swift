// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

// Make sure we can swap two values in an array without retaining anything.

// FIXME: In optimized builds, we inline some stuff, and end up with
// the following instruction sequence -- %130 is a never-returning
// function:
//
// strong_retain %23 : $Builtin.NativeObject
// strong_retain %23 : $Builtin.NativeObject
// strong_retain %23 : $Builtin.NativeObject
// %130 = apply %121(%125, %114, %118, %120, %126)
//
// But really the strong_retains here should be optimized away.

// XFAIL: optimized_stdlib


// CHECK-LABEL: sil @_TF11swap_refcnt11swapByIndex
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
// CHECK: return %
public func swapByIndex(A: inout [Int8], x : Int, y : Int) {
  swap(&A[x],&A[y])
}

