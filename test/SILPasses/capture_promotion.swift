// RUN: %target-swift-frontend %s -emit-sil -o - -verify | FileCheck %s

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

// CHECK: sil hidden @_TF17capture_promotion22test_capture_promotionFT_FT_Si
func test_capture_promotion() -> () -> Int {
  var x : Int = 1
  var y : Foo = Foo()
  var z : Baz = Baz()

// CHECK-NOT: alloc_box

// CHECK: [[CLOSURE0_PROMOTE0:%.*]] = function_ref @_TTSf2d_i_d_i_d_i___TFF17capture_promotion22test_capture_promotionFT_FT_SiU_FT_Si
// CHECK: partial_apply [[CLOSURE0_PROMOTE0]]({{%[0-9]*}}, {{%[0-9]*}}, {{%[0-9]*}})

  return { x + y.foo() + z.x }
}

// CHECK: sil shared @_TTSf2d_i_d_i_d_i___TFF17capture_promotion22test_capture_promotionFT_FT_SiU_FT_Si : $@convention(thin) (Int, @owned Foo, @owned Baz) -> Int

