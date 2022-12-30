// RUN: %target-swift-emit-sil -Onone -verify -enable-experimental-move-only %s | %FileCheck %s

@_moveOnly
class C {}

@_silgen_name("getC")
func getC() -> C

@_silgen_name("takeC")
func takeC(_ c: __owned C)

@_silgen_name("borrowC")
func borrowC(_ c: C)

@_silgen_name("something")
func something()

// Verify that a move-only value's lifetime extends to the end of the
// non-consuming scope where it appears but not further (which would require a
// copy).
//
// CHECK-LABEL: sil hidden @test_diamond__consume_r__use_l : $@convention(thin) (Bool) -> () {
// CHECK:         [[INSTANCE:%[^,]+]] = move_value [lexical] {{%[^,]+}}
// CHECK:         cond_br {{%[^,]+}}, [[LEFT:bb[0-9]+]], [[RIGHT:bb[0-9]+]]
// CHECK:       [[LEFT]]:
// CHECK:         [[BORROW_C:%[^,]+]] = function_ref @borrowC
// CHECK:         apply [[BORROW_C]]([[INSTANCE]])
// CHECK:         [[SOMETHING:%[^,]+]] = function_ref @something
// CHECK:         apply [[SOMETHING]]
// CHECK:         [[DESTROY_C:%[^,]+]] = function_ref @$s17moveonly_lifetime1CCfD
// CHECK:         apply [[DESTROY_C]]([[INSTANCE]])
// CHECK:       [[RIGHT]]:
// CHECK:         [[TAKE_C:%[^,]+]] = function_ref @takeC
// CHECK:         apply [[TAKE_C]]([[INSTANCE]])
// CHECK-LABEL: } // end sil function 'test_diamond__consume_r__use_l'
@_silgen_name("test_diamond__consume_r__use_l")
func test_diamond(_ condition: Bool) {
  let c = getC()
  if condition {
    borrowC(c)
    something()
  } else {
    takeC(c)
  }
}
