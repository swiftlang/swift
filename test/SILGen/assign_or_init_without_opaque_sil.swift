// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -disable-availability-checking -Xllvm -sil-full-demangle -primary-file %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// Extracted from opaque_values_closures.swift.
// This version uses the assign_or_init SILGen instruction instead of the previous assign_by_wrapper.
// Because init accessors do not currently support `-enable-sil-opaque-values`, 
// this test runs without that flag to validate closure behavior in the local context.

@propertyWrapper struct BoxWrapper<T> { var wrappedValue: T }

// CHECK-LABEL: sil {{.*}}[ossa] @$s33assign_or_init_without_opaque_sil35captureBoxNonopaqueOwnedNonescapingyyxyXElF : {{.*}} {
// CHECK:       bb0([[GET:%[^,]+]] :
// CHECK:         [[BOX:%[^,]+]] = alloc_box $<τ_0_0> { var BoxWrapper<τ_0_0> } <U>, var
// CHECK:         [[VAR:%[^,]+]] = mark_uninitialized [var] [[BOX]]
// CHECK:         [[VAR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VAR]]
// CHECK:         [[VAR_ADDR:%[^,]+]] = project_box [[VAR_LIFETIME]]
// TODO: DETERMINE: Considering that captureCanEscape is false, should this mark_function_escape be emitted?
// CHECK:         mark_function_escape [[VAR_ADDR]]
// CHECK:         [[LOCAL:%[^,]+]] = function_ref @$s33assign_or_init_without_opaque_sil35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF
// CHECK:         apply [[LOCAL]]<U>([[VAR_LIFETIME]], [[GET]])
// CHECK:         end_borrow [[VAR_LIFETIME]]
// CHECK:         destroy_value [[VAR]]
// CHECK-LABEL: } // end sil function '$s33assign_or_init_without_opaque_sil35captureBoxNonopaqueOwnedNonescapingyyxyXElF'

// CHECK-LABEL: sil {{.*}}[ossa] @$s33assign_or_init_without_opaque_sil35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF : {{.*}}
// CHECK:       bb0(%0 : @closureCapture @guaranteed $<τ_0_0> { var BoxWrapper<τ_0_0> } <U>, 
// CHECK-SAME:      %1 :
// CHECK-LABEL: } // end sil function '$s33assign_or_init_without_opaque_sil35captureBoxNonopaqueOwnedNonescapingyyxyXElF5localL_yylF'
func captureBoxNonopaqueOwnedNonescaping<U>(_ get: () -> U) {
  @BoxWrapper var u: U

  func local() {
    u = get()
  }
  local()
}
