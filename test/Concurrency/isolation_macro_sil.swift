// RUN: %target-swift-frontend -parse-as-library -emit-sil %s -module-name test | %FileCheck %s

// REQUIRES: concurrency

nonisolated(nonsending) func nonisolatedNonsending() async {
  let iso = #isolation
  take(iso: iso)
}

func take(iso: (any Actor)?) {}

func takeDefaulted(iso: isolated (any Actor)? = #isolation) {}

// CHECK-LABEL: // nonisolatedNonsending()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden @$s4test21nonisolatedNonsendingyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK:      bb0([[ACTOR:%.*]] : $Optional<any Actor>):
// CHECK:   retain_value [[ACTOR]]
// CHECK:   debug_value [[ACTOR]], let, name "iso"
// CHECK:   [[FUNC:%.*]] = function_ref @$s4test4take3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC]]([[ACTOR]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   release_value [[ACTOR]]
// CHECK: } // end sil function '$s4test21nonisolatedNonsendingyyYaF'

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

//   Check that we capture variables as necessary to emit #isolation
//   correctly in defer bodies.
func deferWithIsolatedParam(_ iso: isolated (any Actor)) {
  defer {
    take(iso: #isolation)
  }
  do {}
}
// CHECK-LABEL: sil hidden @$s4test22deferWithIsolatedParamyyScA_pYiF :
// CHECK:       bb0(%0 : $any Actor)
// CHECK:         [[DEFER:%.*]] = function_ref @$s4test22deferWithIsolatedParamyyScA_pYiF6$deferL_yyF :
// CHECK-NEXT:    apply [[DEFER]](%0)

// CHECK-LABEL: sil private @$s4test22deferWithIsolatedParamyyScA_pYiF6$deferL_yyF :
// CHECK:       bb0(%0 : @closureCapture $any Actor):
// CHECK:         [[T0:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, %0
// CHECK:         [[FN:%.*]] = function_ref @$s4test4take3isoyScA_pSg_tF :
// CHECK-NEXT:    apply [[FN]]([[T0]])

//   Check that that happens even with uses in caller-side default
//   arguments, which capture analysis was not previously walking into.
func deferWithIsolatedParam_defaultedUse(_ iso: isolated (any Actor)) {
  defer {
    takeDefaulted()
  }
  do {}
}

// CHECK-LABEL: sil hidden @$s4test35deferWithIsolatedParam_defaultedUseyyScA_pYiF :
// CHECK:       bb0(%0 : $any Actor):
// CHECK:         [[DEFER:%.*]] = function_ref @$s4test35deferWithIsolatedParam_defaultedUseyyScA_pYiF6$deferL_yyF :
// CHECK-NEXT:    apply [[DEFER]](%0)

// CHECK-LABEL: sil private @$s4test35deferWithIsolatedParam_defaultedUseyyScA_pYiF6$deferL_yyF :
// CHECK:       bb0(%0 : @closureCapture $any Actor):
// CHECK:         [[T0:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, %0
// CHECK:         [[FN:%.*]] = function_ref @$s4test13takeDefaulted3isoyScA_pSgYi_tF :
// CHECK-NEXT:    apply [[FN]]([[T0]])

// TODO: we can't currently call nonisolated(nonsending) functions in
// defer bodies because they have to be async, but that should be
// tested here as well.

//   Check that we emit #isolation correctly in defer bodies.
nonisolated(nonsending)
func hasDefer() async {
  defer {
    take(iso: #isolation)
  }
  do {}
}
// CHECK-LABEL: sil hidden @$s4test8hasDeferyyYaF :
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

// CHECK-LABEL: sil hidden @$s4test14hasNestedDeferyyYaF :
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
