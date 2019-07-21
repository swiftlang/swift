// RUN: %target-swift-frontend %s -emit-sil -o - | %FileCheck %s

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

// CHECK: sil hidden @$s17capture_promotion05test_a1_B0SiycyF
func test_capture_promotion() -> () -> Int {
  var x : Int = 1; x = 1
  var y : Foo = Foo(); y = Foo()
  var z : Baz = Baz(); z = Baz()

// CHECK-NOT: alloc_box

// CHECK: [[CLOSURE0_PROMOTE0:%.*]] = function_ref @$s17capture_promotion05test_a1_B0SiycyFSiycfU_Tf2iii_n
// CHECK: partial_apply [callee_guaranteed] [[CLOSURE0_PROMOTE0]]({{%[0-9]*}}, {{%[0-9]*}}, {{%[0-9]*}})

  return { x + y.foo() + z.x }
}

// CHECK: sil private @$s17capture_promotion05test_a1_B0SiycyFSiycfU_Tf2iii_n : $@convention(thin) (Int, @guaranteed Foo, @guaranteed Baz) -> Int

