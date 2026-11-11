// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature OnceFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

// Exactly-once parameters are `@owned` and `@callee_owned`, just like
// at-most-once parameters, and calling one consumes it.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr19testCallExactlyOnceyyyyXEnF : $@convention(thin) (@owned @noescape @once @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@noescape @once @callee_owned () -> ()):
// CHECK:   [[VALUE:%.*]] = load [copy] {{%.*}} : $*@noescape @once @callee_owned () -> ()
// CHECK:   apply [[VALUE]]() : $@noescape @once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr19testCallExactlyOnceyyyyXEnF'
func testCallExactlyOnce(_ f: @once () -> Void) {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s9once_attr27testCallExactlyOnceEscapingyyyyXOnF : $@convention(thin) (@owned @once @callee_owned () -> ()) -> () {
// CHECK:   apply {{%.*}}() : $@once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr27testCallExactlyOnceEscapingyyyyXOnF'
func testCallExactlyOnceEscaping(_ f: @escaping @once () -> Void) {
  f()
}

// A captureless closure is formed directly by `thin_to_thick_function`. The
// closure body itself is a plain thin function.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr15makeCapturelessyyXOyF : $@convention(thin) () -> @owned @once @callee_owned () -> () {
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s9once_attr15makeCapturelessyyXOyFyyXOfU_ : $@convention(thin) () -> ()
// CHECK:   [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) () -> () to $@once @callee_owned () -> ()
// CHECK:   return [[THICK]] : $@once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr15makeCapturelessyyXOyF'
func makeCaptureless() -> @once () -> Void {
  return { }
}

// A closure that captures an exactly-once value consumes it.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr13makeCapturingyyyXOyyXOnF : $@convention(thin) (@owned @once @callee_owned () -> ()) -> @owned @once @callee_owned () -> () {
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s9once_attr13makeCapturingyyyXOyyXOnFyyXOfU_ : $@convention(thin) (@owned @once @callee_owned () -> ()) -> ()
// CHECK:   [[CAPTURE:%.*]] = load [take] {{%.*}} : $*@once @callee_owned () -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called_once] [[CLOSURE]]([[CAPTURE]]) : $@convention(thin) (@owned @once @callee_owned () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr13makeCapturingyyyXOyyXOnF'

// CHECK-LABEL: sil private [ossa] @$s9once_attr13makeCapturingyyyXOyyXOnFyyXOfU_ : $@convention(thin) (@owned @once @callee_owned () -> ()) -> () {
// CHECK: bb0({{%.*}} : @closureCapture @owned $@once @callee_owned () -> ()):
// CHECK: } // end sil function '$s9once_attr13makeCapturingyyyXOyyXOnFyyXOfU_'
func makeCapturing(_ f: @escaping @once () -> Void) -> @once () -> Void {
  return { @once in f() }
}

// Converting a plain function to an exactly-once function goes through a
// thunk.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr13makeFromPlainyyyXOyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @owned @once @callee_owned () -> () {
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeg_IeOx_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called_once] [[THUNK]]({{%.*}}) : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr13makeFromPlainyyyXOyycF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_IeOx_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   apply [[FN]]() : $@callee_guaranteed () -> ()
// CHECK: } // end sil function '$sIeg_IeOx_TR'
func makeFromPlain(_ f: @escaping () -> Void) -> @once () -> Void {
  return f
}

// Converting an at-most-once function to an exactly-once function also goes
// through a thunk, even though the two have the same ABI, so that every
// exactly-once value has a context of its own. The thunk consumes the source
// value when it calls it.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr18makeFromAtMostOnceyyyXOyyXOonF : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> @owned @once @callee_owned () -> () {
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeOox_IeOx_TR : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called_once] [[THUNK]]({{%.*}}) : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr18makeFromAtMostOnceyyyXOyyXOonF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeOox_IeOx_TR : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[FN:%.*]] : @owned $@callable(once) @callee_owned () -> ()):
// CHECK:   apply [[FN]]() : $@callable(once) @callee_owned () -> ()
// CHECK: } // end sil function '$sIeOox_IeOx_TR'
func makeFromAtMostOnce(_ f: @escaping @callable(once) () -> Void) -> @once () -> Void {
  return f
}

// A witness with an exactly-once parameter can satisfy a requirement with an
// at-most-once parameter. The witness thunk converts the argument through a
// thunk, so that the witness gets a context of its own.
protocol RunsAtMostOnce {
  func run(_ f: @escaping @callable(once) () -> Void)
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s9once_attr6RunnerVAA14RunsAtMostOnceA2aDP3runyyyyXOonFTW : $@convention(witness_method: RunsAtMostOnce) (@owned @callable(once) @callee_owned () -> (), @in_guaranteed Runner) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@callable(once) @callee_owned () -> (), {{%.*}} : $*Runner):
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeOox_IeOx_TR : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
// CHECK:   [[CONVERTED:%.*]] = partial_apply [called_once] [[THUNK]]([[F]]) : $@convention(thin) (@owned @callable(once) @callee_owned () -> ()) -> ()
// CHECK:   [[WITNESS:%.*]] = function_ref @$s9once_attr6RunnerV3runyyyyXOnF : $@convention(method) (@owned @once @callee_owned () -> (), Runner) -> ()
// CHECK:   apply [[WITNESS]]([[CONVERTED]], {{%.*}})
// CHECK: } // end sil function '$s9once_attr6RunnerVAA14RunsAtMostOnceA2aDP3runyyyyXOonFTW'
struct Runner: RunsAtMostOnce {
  func run(_ f: @escaping @once () -> Void) { f() }
}

// A borrowed exactly-once parameter is `@guaranteed`, and borrowing it doesn't
// consume it.

func inspect(_ f: borrowing @once () -> Void) {}
func run(_ body: () -> Void) { body() }

// CHECK-LABEL: sil hidden [ossa] @$s9once_attr17borrowedForwardedyyyyXEF : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @guaranteed $@noescape @once @callee_owned () -> ()):
// CHECK:   [[COPY:%.*]] = copy_value [[F]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
// CHECK:   [[BORROW:%.*]] = begin_borrow [[MARK]]
// CHECK:   [[INSPECT:%.*]] = function_ref @$s9once_attr7inspectyyyyXEF
// CHECK:   apply [[INSPECT]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK: } // end sil function '$s9once_attr17borrowedForwardedyyyyXEF'
func borrowedForwarded(_ f: borrowing @once () -> Void) {
  inspect(f)
}

// An owned value can be borrowed before it's called.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr18borrowedThenCalledyyyyXEnF : $@convention(thin) (@owned @noescape @once @callee_owned () -> ()) -> () {
// CHECK:   [[INSPECT:%.*]] = function_ref @$s9once_attr7inspectyyyyXEF
// CHECK:   apply [[INSPECT]]({{%.*}}) : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> ()
// CHECK:   apply {{%.*}}() : $@noescape @once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr18borrowedThenCalledyyyyXEnF'
func borrowedThenCalled(_ f: @once () -> Void) {
  inspect(f)
  f()
}

// An ordinary nonescaping closure can borrow a captured exactly-once value.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr28borrowedByNonescapingClosureyyyyXEF : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> () {
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s9once_attr28borrowedByNonescapingClosureyyyyXEFyyXEfU_ : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> ()
// CHECK:   [[CAPTURE:%.*]] = copy_value {{%.*}} : $@noescape @once @callee_owned () -> ()
// CHECK:   partial_apply [callee_guaranteed] [[CLOSURE]]([[CAPTURE]]) : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> ()
// CHECK: } // end sil function '$s9once_attr28borrowedByNonescapingClosureyyyyXEF'

// CHECK-LABEL: sil private [ossa] @$s9once_attr28borrowedByNonescapingClosureyyyyXEFyyXEfU_ : $@convention(thin) (@guaranteed @noescape @once @callee_owned () -> ()) -> () {
// CHECK: bb0({{%.*}} : @closureCapture @guaranteed $@noescape @once @callee_owned () -> ()):
// CHECK:   mark_unresolved_non_copyable_value [no_consume_or_assign]
// CHECK: } // end sil function '$s9once_attr28borrowedByNonescapingClosureyyyyXEFyyXEfU_'
func borrowedByNonescapingClosure(_ f: borrowing @once () -> Void) {
  run { inspect(f) }
}

// The type of a function value doesn't depend on the value, and its isolation
// is extracted from a borrow.
// CHECK-LABEL: sil hidden [ossa] @$s9once_attr15borrowedQueriesyScA_pSgyyYAXOF : $@convention(thin) (@guaranteed @isolated(any) @once @callee_owned () -> ()) -> @owned Optional<any Actor> {
// CHECK:   metatype $@thin (@isolated(any) @once () -> ()).Type
// CHECK:   function_extract_isolation {{%.*}} : $@isolated(any) @once @callee_owned () -> ()
// CHECK: } // end sil function '$s9once_attr15borrowedQueriesyScA_pSgyyYAXOF'
func borrowedQueries(
  _ f: borrowing @escaping @isolated(any) @once () -> Void
) -> (any Actor)? {
  _ = type(of: f)
  return f.isolation
}
