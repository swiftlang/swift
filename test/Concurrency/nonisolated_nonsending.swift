// RUN: %target-swift-frontend -emit-sil -parse-as-library -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

protocol P: Sendable {
  func f() async
}

// Our map closure (abstracted as returning T) returns a function
// value, which requires it to be reabstracted to the most general
// abstraction pattern, i.e. from
//     nonisolated(nonsending) () async -> ()
// to
//     nonisolated(nonsending) () async -> U, where U == ()
// Note that we preserve nonisolated(nonsending).
//
// The thunk code did not expect the output function type to be
// nonisolated(nonsending), so it didn't handle and propagate the
// implicit isolation argument correctly.
func testPartialApplication(p: [any P]) async {
  _ = p.map { $0.f }
}
// CHECK-LABEL: sil private @$s22nonisolated_nonsending22testPartialApplication1pySayAA1P_pG_tYaFyyYaYbYCcAaD_pXEfU_ :
// CHECK: function_ref @$sScA_pSgIeghHgIL_AAytIeghHgILr_TR :

//   Reabstraction thunk from caller-isolated () -> () to caller-isolated () -> T
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @$sScA_pSgIeghHgIL_AAytIeghHgILr_TR :
// CHECK:       bb0(%0 : $*(), %1 : $Optional<any Actor>, %2 : $@Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()):
// CHECK-NEXT:    apply %2(%1) :

func takesGenericAsyncFunction<T>(_ fn: nonisolated(nonsending) (T) async -> Void) {}

// This is a more direct test of the partial-application code above.
// Note that we have to make the functions explicitly nonisolated(nonsending)
// because NonisolatedNonsendingByDefault only applies to declarations,
// not function types in the abstract.
func testReabstractionPreservingCallerIsolation(fn: nonisolated(nonsending) (Int) async -> Void) {
  takesGenericAsyncFunction(fn)
}
// CHECK-LABEL: sil hidden @$s22nonisolated_nonsending42testReabstractionPreservingCallerIsolation2fnyySiYaYCXE_tF :
// CHECK:       bb0(%0 : $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, Int) -> ()):
// CHECK:         [[THUNK:%.*]] = function_ref @$sScA_pSgSiIgHgILy_AASiIegHgILn_TR :

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @$sScA_pSgSiIgHgILy_AASiIegHgILn_TR :
// CHECK:       bb0(%0 : $Optional<any Actor>, %1 : $*Int, %2 : $@noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, Int) -> ()):
// CHECK-NEXT:    %3 = load %1
// CHECK-NEXT:    apply %2(%0, %3)

func takesAsyncIsolatedAnyFunction(_ fn: @isolated(any) () async -> Void) {}
func takesGenericAsyncIsolatedAnyFunction<T>(_ fn: @isolated(any) (T) async -> Void) {}

// These would be good to test, but we apparently reject this conversion.
#if false

// The same bug, but converting to an @isolated(any) function type.
func testConversionToIsolatedAny(fn: nonisolated(nonsending) () async -> Void) {
  takesAsyncIsolatedAnyFunction(fn)
}

func testReabstractionToIsolatedAny(fn: nonisolated(nonsending) (Int) async -> Void) {
  takesGenericAsyncIsolatedAnyFunction(fn)
}

#endif
