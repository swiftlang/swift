// RUN: %target-swift-emit-sil -sil-verify-all -module-name moveonly_lifetime -o /dev/null -Xllvm -sil-print-canonical-module -Onone -verify -enable-experimental-move-only %s 2>&1 | %FileCheck %s

@_moveOnly
struct C {
    var i = 5

    deinit {}
}

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
// CHECK-LABEL: sil hidden [ossa] @test_diamond__consume_r__use_l : $@convention(thin) (Bool) -> () {
// CHECK: bb0(
// CHECK:   [[STACK:%.*]] = alloc_stack
// CHECK:   [[VALUE:%.*]] = apply
// CHECK:   store [[VALUE]] to [init] [[STACK]]
// CHECK:   cond_br {{%[^,]+}}, [[LEFT:bb[0-9]+]], [[RIGHT:bb[0-9]+]]
//
// CHECK:   [[RIGHT]]:
// CHECK:     [[INSTANCE:%.*]] = load [take] [[STACK]]
// CHECK:     [[TAKE_C:%[^,]+]] = function_ref @takeC
// CHECK:     apply [[TAKE_C]]([[INSTANCE]])
//
// NOTE: We shrink the lifetime too far below. This needs the move checker to
// begin to maximize lifetimes.
//
// CHECK:   [[LEFT]]:
// CHECK:     [[INSTANCE:%.*]] = load_borrow [[STACK]]
// CHECK:     [[BORROW_C:%[^,]+]] = function_ref @borrowC
// CHECK:     apply [[BORROW_C]]([[INSTANCE]])
// CHECK:     end_borrow [[INSTANCE]]
// CHECK:     [[FUNC:%.*]] = function_ref @$s17moveonly_lifetime1CVfD : $@convention(method) (@owned C) -> ()
// CHECK:     [[INSTANCE:%.*]] = load [take] [[STACK]]
// CHECK:     apply [[FUNC]]([[INSTANCE]])
// CHECK:     [[SOMETHING:%[^,]+]] = function_ref @something
// CHECK:     apply [[SOMETHING]]
// CHECK: } // end sil function 'test_diamond__consume_r__use_l'
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
REQUIRES: updating_for_owned_noescape
