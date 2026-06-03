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
// CHECK: function_ref @$sBAIeNghHgIL_BAytIeNghHgILr_TR :

//   Reabstraction thunk from caller-isolated () -> () to caller-isolated () -> T
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @$sBAIeNghHgIL_BAytIeNghHgILr_TR : $@convention(thin) @caller_isolated @Sendable @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @caller_isolated @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()) -> @out () {
// CHECK:       bb0(%0 : $*(), %1 : $Builtin.ImplicitActor, %2 : $@caller_isolated @Sendable @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()):
// The hop is eliminated because the argument is `nonisolated(nonsending)`
// CHECK-NOT:    hop_to_executor %1
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
// CHECK:       bb0(%0 : $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> ()):
// CHECK:         [[THUNK:%.*]] = function_ref @$sBASiINgHgILy_BASiIeNgHgILn_TR : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Int, @guaranteed @caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> ()) -> ()

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @$sBASiINgHgILy_BASiIeNgHgILn_TR : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Int, @guaranteed @caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> ()) -> () {
// CHECK:       bb0(%0 : $Builtin.ImplicitActor, %1 : $*Int, %2 : $@caller_isolated @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, Int) -> ()):
// CHECK-NEXT:    %3 = load %1
// The hop is eliminated because the argument is `nonisolated(nonsending)`
// CHECK-NOT:    hop_to_executor %0
// CHECK-NEXT:    apply %2(%0, %3)

func takesAsyncIsolatedAnyFunction(_ fn: @isolated(any) () async -> Void) {}
func takesGenericAsyncIsolatedAnyFunction<T>(_ fn: @isolated(any) (T) async -> Void) {}

func testReabstractionHopsBack() async throws {
  func compute<T, R>(_ fn: nonisolated(nonsending) (T) async -> R?) async {
  }

  func computeThrowing(_ fn: nonisolated(nonsending) () async throws -> Void) async throws {
  }

  // CHECK: // thunk for @callee_guaranteed @async (@unowned Int) -> (@unowned ()?)
  // CHECK: // Isolation: nonisolated(nonsending)
  // CHECK: sil shared [transparent] [reabstraction_thunk] @$sSiytSgIgHyd_BASiAAIeNgHgILnr_TR : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed Int, @guaranteed @noescape @async @callee_guaranteed (Int) -> Optional<()>) -> @out Optional<()> {
  // CHECK: bb0([[RESULT:%.*]] : $*Optional<()>, [[ISOLATION:%.*]] : $Builtin.ImplicitActor, [[X_REF:%.*]] : $*Int, [[CLOSURE:%.*]] : $@noescape @async @callee_guaranteed (Int) -> Optional<()>):
  // CHECK:   [[X:%.*]] = load [[X_REF]]
  // CHECK:   [[CLOSURE_RESULT:%.*]] = apply [[CLOSURE]]([[X]]) : $@noescape @async @callee_guaranteed (Int) -> Optional<()>
  // CHECK:   store [[CLOSURE_RESULT]] to [[RESULT]]
  // CHECK:   hop_to_executor [[ISOLATION]]
  // CHECK: } // end sil function '$sSiytSgIgHyd_BASiAAIeNgHgILnr_TR'
  await compute { @concurrent (x: Int) in
      _ = x
  }

  struct MyError: Error {
  }

  // CHECK: // thunk for @callee_guaranteed @Sendable @async () -> (@error @owned Error)
  // CHECK: // Isolation: nonisolated(nonsending)
  // CHECK: sil shared [transparent] [reabstraction_thunk] @$ss5Error_pIghHzo_BAsAA_pIeNgHgILzo_TR : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed @noescape @Sendable @async @callee_guaranteed () -> @error any Error) -> @error any Error {
  // CHECK: bb0([[ISOLATION:%.*]] : $Builtin.ImplicitActor, [[CLOSURE:%.*]] : $@noescape @Sendable @async @callee_guaranteed () -> @error any Error):
  // CHECK:   try_apply [[CLOSURE]]() : $@noescape @Sendable @async @callee_guaranteed () -> @error any Error, normal bb1, error bb2
  // CHECK: bb1({{.*}} : $()):
  // CHECK:   hop_to_executor [[ISOLATION]]
  // CHECK: bb2({{.*}} : $any Error):
  // CHECK:   hop_to_executor [[ISOLATION]]
  // CHECK: } // end sil function '$ss5Error_pIghHzo_BAsAA_pIeNgHgILzo_TR'
  try await computeThrowing { @MainActor in
      throw MyError()
  }
}

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
