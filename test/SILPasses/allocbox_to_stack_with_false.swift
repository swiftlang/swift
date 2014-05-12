// RUN: %swift -emit-sil %s | FileCheck %s

// Make sure that materializations of computed properties such as 'false' and
// 'true' don't leave needless stack allocations. <rdar://problem/15272642>

// CHECK-LABEL: sil @_TF28allocbox_to_stack_with_false1gFT_T_ : $@thin () -> () {
// CHECK-NOT: alloc_stack
// CHECK-NOT: dealloc_stack
// CHECK: }
func g() {
  if false {}
}

// Verify we don't crash on this.
// rdar://15595118
operator infix ~> { precedence 255 }
protocol Target {}
func ~> <Target1>(inout x: Int, f: (inout _: Int, _: Target)->Target) -> (Target)->Target {
  return { f(&x, $0) }
}

