// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -Xllvm -sil-print-types -emit-sil -Onone -module-name=test -Xllvm -sil-print-after=address-lowering -o /dev/null %s 2>&1 | %FileCheck %s --check-prefix=LOWERED
// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -Xllvm -sil-print-types -emit-sil -Onone -module-name=test %s | %FileCheck %s

// `copy` of a borrowed generic value emits `explicit_copy_value`, which
// AddressLowering must rewrite to `explicit_copy_addr`.
//
// This is deliberately independent of C++ interop: @cxx @implementation also
// exercises this path, but that feature may change.

// Immediately after AddressLowering, the copy is still explicit.
// LOWERED-LABEL: sil [ossa] @${{.*}}6assign{{.*}} : $@convention(thin) <T> (@inout T, @in_guaranteed T) -> () {
// LOWERED:         [[TMP:%[^,]+]] = alloc_stack $T
// LOWERED-NOT:     explicit_copy_value
// LOWERED:         explicit_copy_addr %{{[^,]+}} to [init] [[TMP]] : $*T
// LOWERED-NOT:     explicit_copy_value
// LOWERED:         copy_addr [take] [[TMP]] to %{{[^,]+}} : $*T
// LOWERED-LABEL: } // end sil function '${{.*}}6assign{{.*}}'

// By the end of the pipeline it is an ordinary copy_addr into the temporary,
// which is then moved into the inout destination.
// CHECK-LABEL: sil @${{.*}}6assign{{.*}} : $@convention(thin) <T> (@inout T, @in_guaranteed T) -> () {
// CHECK:       bb0(%0 : $*T, %1 : @noImplicitCopy $*T):
// CHECK:         [[TMP:%[^,]+]] = alloc_stack $T
// CHECK-NOT:     explicit_copy_value
// CHECK:         copy_addr %1 to [init] [[TMP]] : $*T
// CHECK:         copy_addr [take] [[TMP]] to %{{[^,]+}} : $*T
// CHECK-LABEL: } // end sil function '${{.*}}6assign{{.*}}'
public func assign<T>(_ dst: inout T, _ src: borrowing T) {
  dst = copy src
}
