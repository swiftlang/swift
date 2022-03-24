// RUN: %target-swift-frontend -enable-sil-opaque-values -parse-as-library -emit-sil -Onone %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s19opaque_values_Onone16generic_identity1txx_tlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : $*T, %1 : $*T):
// CHECK:   debug_value %1 : $*T, let, name "t", argno 1
// CHECK:   copy_addr %1 to [initialization] %0 : $*T
// CHECK-LABEL: } // end sil function '$s19opaque_values_Onone16generic_identity1txx_tlF'
func generic_identity<T>(t: T) -> T {
  return t
}
