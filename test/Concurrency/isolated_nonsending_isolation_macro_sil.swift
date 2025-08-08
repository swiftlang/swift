// RUN: %target-swift-frontend -parse-as-library -emit-sil %s | %FileCheck %s

// REQUIRES: concurrency

nonisolated(nonsending) func nonisolatedNonsending() async {
  let iso = #isolation
  take(iso: iso)
}

func take(iso: (any Actor)?) {}

// CHECK-LABEL: // nonisolatedNonsending()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden @$s39isolated_nonsending_isolation_macro_sil21nonisolatedNonsendingyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitIsolationActor) -> () {
// CHECK:      bb0([[ACTOR:%.*]] : $Builtin.ImplicitIsolationActor):
// CHECK:   [[ACTOR_CAST:%.*]] = unchecked_bitwise_cast [[ACTOR]] to $Optional<any Actor>
// CHECK:   retain_value [[ACTOR_CAST]]
// CHECK:   debug_value [[ACTOR_CAST]], let, name "iso"
// CHECK:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil4take3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC]]([[ACTOR_CAST]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   release_value [[ACTOR_CAST]]
// CHECK: } // end sil function '$s39isolated_nonsending_isolation_macro_sil21nonisolatedNonsendingyyYaF'
