// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -Xllvm -sil-full-demangle %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// REQUIRES: objc_interop

import ObjectiveC

// CHECK-LABEL: sil {{.*}} @$s18opaque_values_objc3fooyyFyypSgcfU_To : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[INSTANCE:%[^,]+]] : @owned $Optional<Any>
// CHECK:         [[GUARANTEED:%[^,]+]] = begin_borrow [[INSTANCE]]
// CHECK:         [[FN:%[^,]+]] = function_ref @$s18opaque_values_objc3fooyyFyypSgcfU_
// CHECK:         apply [[FN]]([[GUARANTEED]])
// CHECK:         end_borrow [[GUARANTEED]]
// CHECK:         destroy_value [[INSTANCE]]
// CHECK-LABEL: } // end sil function '$s18opaque_values_objc3fooyyFyypSgcfU_To'
func foo() {
  objc_setUncaughtExceptionHandler { _ in
  }
}
REQUIRES: updating_for_owned_noescape
