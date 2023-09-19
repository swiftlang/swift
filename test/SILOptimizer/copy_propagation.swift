// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

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
@_silgen_name("test_hoist_over_non_barrier")
func test_hoist_over_non_barrier() {
  let c = C()
  borrow(c)
  non_barrier()
}

