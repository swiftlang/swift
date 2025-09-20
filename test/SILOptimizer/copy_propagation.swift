// RUN: %target-swift-frontend -primary-file %s -O \
// RUN:   -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// RUN: %target-swift-frontend -primary-file %s -Onone -enable-copy-propagation \
// RUN:    -sil-verify-all -module-name=test -emit-sil | %FileCheck %s --check-prefix ONONE

// REQUIRES: swift_in_compiler

class C {}

@_silgen_name("non_barrier")
@inline(never)
func non_barrier() {}

@_silgen_name("borrow")
@inline(never)
func borrow(_ c: C)

// CHECK-LABEL: sil {{.*}}@test_hoist_over_non_barrier : {{.*}} {
// CHECK:         [[INSTANCE:%[^,]+]] = alloc_ref
// CHECK:         [[EI:%.*]] = end_init_let_ref [[INSTANCE]]
// CHECK:         [[BORROW:%[^,]+]] = function_ref @borrow
// CHECK:         apply [[BORROW]]([[EI]])
// CHECK:         strong_release [[EI]]
// CHECK:         [[NON_BARRIER:%[^,]+]] = function_ref @non_barrier
// CHECK:         apply [[NON_BARRIER]]()
// CHECK-LABEL: } // end sil function 'test_hoist_over_non_barrier'


/////
// NOTE: in -Onone, the release is NOT hoisted, to preserve debugging.

// ONONE-LABEL: sil {{.*}}@test_hoist_over_non_barrier : {{.*}} {
// ONONE:         [[INSTANCE:%[^,]+]] = apply {{.*}} -> @owned C
// ONONE:         debug_value [[INSTANCE]], let, name "c"
// ONONE:         [[BORROW:%[^,]+]] = function_ref @borrow
// ONONE:         apply [[BORROW]]([[INSTANCE]])
// ONONE:         [[NON_BARRIER:%[^,]+]] = function_ref @non_barrier
// ONONE:         apply [[NON_BARRIER]]()
// ONONE:         strong_release [[INSTANCE]]
// ONONE-LABEL: } // end sil function 'test_hoist_over_non_barrier'
@_silgen_name("test_hoist_over_non_barrier")
func test_hoist_over_non_barrier() {
  let c = C()
  borrow(c)
  non_barrier()
}

// CHECK-LABEL: sil {{.*}}@reassign_with_lets : {{.*}} {
// CHECK:           [[INSTANCE:%[^,]+]] = alloc_ref $C
// CHECK:           [[EI:%.*]] = end_init_let_ref [[INSTANCE]]
// CHECK-NEXT:      debug_value [[EI]], let, name "x"
// CHECK-NEXT:      debug_value [[EI]], let, name "y"
// CHECK-NEXT:      debug_value [[EI]], let, name "z"
// CHECK-NEXT:      return [[EI]]
// CHECK-LABEL: } // end sil function 'reassign_with_lets'

// ONONE-LABEL: sil {{.*}}@reassign_with_lets : {{.*}} {
// ONONE:           [[INSTANCE:%[^,]+]] = apply {{.*}} -> @owned C
// ONONE-NEXT:      debug_value [[INSTANCE]], let, name "x"
// ONONE-NEXT:      debug_value [[INSTANCE]], let, name "y"
// ONONE-NEXT:      debug_value [[INSTANCE]], let, name "z"
// ONONE-NEXT:      return [[INSTANCE]]
// ONONE-LABEL: } // end sil function 'reassign_with_lets'
@_silgen_name("reassign_with_lets")
func reassign_with_lets() -> C {
  let x = C()
  let y = x
  let z = y
  return z
}

// CHECK-LABEL: sil {{.*}}@renamed_return : {{.*}} {
// CHECK-NOT:       strong_retain
// CHECK:           debug_value %1, let, name "a"
// CHECK-NEXT:      debug_value %1, let, name "b"
// CHECK-NEXT:      debug_value %1, let, name "c"
// CHECK-NEXT:      strong_retain %1
// CHECK-NEXT:      return %1
// CHECK-LABEL: } // end sil function 'renamed_return'


/////
// Slightly different control-flow in -Onone, but still one
// retain dynamically executed on the object.

// ONONE-LABEL: sil {{.*}}@renamed_return : {{.*}} {
// ONONE-NOT:       strong_retain
// ONONE:           debug_value %1, let, name "a"
// ONONE-NEXT:      debug_value %1, let, name "b"
// ONONE-NEXT:      debug_value %1, let, name "c"
// ONONE: cond_br {{.*}} bb1, bb2

// ONONE: bb1
// ONONE:           strong_retain %1
// ONONE:           br bb3

// ONONE: bb2
// ONONE:           strong_retain %1
// ONONE:           br bb3

// ONONE: bb3
// ONONE-NEXT:      return
// ONONE-LABEL: } // end sil function 'renamed_return'
@_silgen_name("renamed_return")
func renamed_return(_ cond: Bool, _ a: C) -> C {
  let b = a
  let c = b
  if cond { return b }
  return c
}

