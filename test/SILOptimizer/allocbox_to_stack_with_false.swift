// RUN: %target-swift-frontend -emit-sil %s | FileCheck %s

// Make sure that materializations of computed properties such as 'false' and
// 'true' don't leave needless stack allocations. <rdar://problem/15272642>

// CHECK-LABEL: sil hidden @_TF28allocbox_to_stack_with_false1gFT_T_ : $@convention(thin) () -> () {
// CHECK-NOT: alloc_stack
// CHECK-NOT: dealloc_stack
// CHECK: }
func g() {
  if false {}
}

// Verify we don't crash on this.
// rdar://15595118
infix operator ~> { precedence 255 }
protocol Target {}

func ~> <Target, Arg0, Result>(inout x: Target, f: (inout _: Target, _: Arg0)->Result) -> (Arg0)->Result {
  return { f(&x, $0) }
}

func ~> (inout x: Int, f: (inout _: Int, _: Target)->Target) -> (Target)->Target {
  return { f(&x, $0) }
}

