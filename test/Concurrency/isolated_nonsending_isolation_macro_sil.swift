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

//   Check that we emit #isolation correctly in closures.
// CHECK-LABEL: // closure #1 in containsClosure()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK-NEXT:    // function_ref take(iso:)
// CHECK-NEXT:    [[FN:%.*]] = function_ref @
// CHECK-NEXT:    apply [[FN]](%0)
func containsClosure() {
  let closure: nonisolated(nonsending) () async -> Void = {
    take(iso: #isolation)
  }
}

//   Check that we emit #isolation correctly in defer bodies.
nonisolated(nonsending)
func hasDefer() async {
  defer {
    take(iso: #isolation)
  }
  do {}
}
// CHECK-LABEL: sil hidden @$s39isolated_nonsending_isolation_macro_sil8hasDeferyyYaF :
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK:         // function_ref $defer
// CHECK-NEXT:    [[DEFER:%.*]] = function_ref
// CHECK-NEXT:    apply [[DEFER]](%0)

// CHECK-LABEL: // $defer #1 () in hasDefer()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK-NEXT:    // function_ref take(iso:)
// CHECK-NEXT:    [[FN:%.*]] = function_ref @
// CHECK-NEXT:    apply [[FN]](%0)

//   Check that we emit #isolation correctly in nested defer bodies.
nonisolated(nonsending)
func hasNestedDefer() async {
  defer {
    defer {
      take(iso: #isolation)
    }
    do {}
  }
  do {}
}

// CHECK-LABEL: sil hidden @$s39isolated_nonsending_isolation_macro_sil14hasNestedDeferyyYaF :
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK:         // function_ref $defer #1 () in hasNestedDefer()
// CHECK-NEXT:    [[DEFER:%.*]] = function_ref
// CHECK-NEXT:    apply [[DEFER]](%0)

// CHECK-LABEL: // $defer #1 () in hasNestedDefer()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK:         // function_ref $defer #1 () in $defer #1 () in hasNestedDefer()
// CHECK-NEXT:    [[DEFER:%.*]] = function_ref
// CHECK-NEXT:    apply [[DEFER]](%0)

// CHECK-LABEL: // $defer #1 () in $defer #1 () in hasNestedDefer()
// CHECK-NEXT:  // Isolation: caller_isolation_inheriting
// CHECK:       bb0(%0 : $Optional<any Actor>):
// CHECK-NEXT:    // function_ref take(iso:)
// CHECK-NEXT:    [[FN:%.*]] = function_ref @
// CHECK-NEXT:    apply [[FN]](%0)
