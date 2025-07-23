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
// CHECK:      bb0([[ACTOR:%.*]] : $Optional<any Actor>):
// CHECK:   retain_value [[ACTOR]]
// CHECK:   debug_value [[ACTOR]], let, name "iso"
// CHECK:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil4take3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC]]([[ACTOR]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   release_value [[ACTOR]]
// CHECK: } // end sil function '$s39isolated_nonsending_isolation_macro_sil21nonisolatedNonsendingyyYaF'
