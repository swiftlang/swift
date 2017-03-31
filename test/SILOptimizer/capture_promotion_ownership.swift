// RUN: %target-swift-frontend %s -enable-sil-ownership -disable-sil-linking -emit-sil -o - -verify | %FileCheck %s

// NOTE: We add -disable-sil-linking to the compile line to ensure that we have
// access to declarations for standard library types, but not definitions. This
// ensures that we are able to safely use standard library types for this test
// without needing the standard library to be ownership correct in its body.

class Foo {
  func foo() -> Int {
    return 1
  }
}

class Bar {
}

struct Baz {
  var bar = Bar()
  var x = 42
}

// CHECK: sil hidden @_T027capture_promotion_ownership05test_a1_B0SiycyF
func test_capture_promotion() -> () -> Int {
  var x : Int = 1; x = 1
  var y : Foo = Foo(); y = Foo()
  var z : Baz = Baz(); z = Baz()

// CHECK-NOT: alloc_box

// CHECK: [[CLOSURE0_PROMOTE0:%.*]] = function_ref @_T027capture_promotion_ownership05test_a1_B0SiycyFSiycfU_Tf2iii_n
// CHECK: partial_apply [[CLOSURE0_PROMOTE0]]({{%[0-9]*}}, {{%[0-9]*}}, {{%[0-9]*}})

  return { x + y.foo() + z.x }
}

// CHECK: sil private @_T027capture_promotion_ownership05test_a1_B0SiycyFSiycfU_Tf2iii_n : $@convention(thin) (Int, @owned Foo, @owned Baz) -> Int

