// RUN: %target-swift-frontend -parse-as-library -emit-sil %s | %FileCheck %s

// REQUIRES: concurrency

nonisolated(nonsending) func nonisolatedNonsending() async {
  let iso = #isolation
  take(iso: iso)
}

func take(iso: (any Actor)?) {}

// CHECK-LABEL: // nonisolatedNonsending()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden @$s39isolated_nonsending_isolation_macro_sil21nonisolatedNonsendingyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK:      bb0(%0 : $Optional<any Actor>):
// CHECK-NEXT:   hop_to_executor %0 // id: %1
// CHECK-NEXT:   retain_value %0 // id: %2
// CHECK-NEXT:   debug_value %0, let, name "iso" // id: %3
// CHECK-NEXT:   // function_ref take(iso:)
// CHECK-NEXT:   %4 = function_ref @$s39isolated_nonsending_isolation_macro_sil4take3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> () // user: %5
// CHECK-NEXT:   %5 = apply %4(%0) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK-NEXT:   release_value %0 // id: %6
// CHECK-NEXT:   %7 = tuple () // user: %8
// CHECK-NEXT:   return %7 // id: %8
// CHECK-NEXT: } // end sil function '$s39isolated_nonsending_isolation_macro_sil21nonisolatedNonsendingyyYaF'