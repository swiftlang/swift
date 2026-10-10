// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

// `@called(exactlyOnce)` parameters are `@owned` and `@callee_owned`, just like
// `@called(atMostOnce)` parameters, and calling one consumes it.
// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once19testCallExactlyOnceyyyyXEnF : $@convention(thin) (@owned @noescape @called(exactlyOnce) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@noescape @called(exactlyOnce) @callee_owned () -> ()):
// CHECK:   [[VALUE:%.*]] = load [copy] {{%.*}} : $*@noescape @called(exactlyOnce) @callee_owned () -> ()
// CHECK:   apply [[VALUE]]() : $@noescape @called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once19testCallExactlyOnceyyyyXEnF'
func testCallExactlyOnce(_ f: @called(exactlyOnce) () -> Void) {
  f()
}

// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once27testCallExactlyOnceEscapingyyyyXOnF : $@convention(thin) (@owned @called(exactlyOnce) @callee_owned () -> ()) -> () {
// CHECK:   apply {{%.*}}() : $@called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once27testCallExactlyOnceEscapingyyyyXOnF'
func testCallExactlyOnceEscaping(_ f: @escaping @called(exactlyOnce) () -> Void) {
  f()
}

// A captureless closure is formed directly by `thin_to_thick_function`. The
// closure body itself is a plain thin function.
// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once15makeCapturelessyyXOyF : $@convention(thin) () -> @owned @called(exactlyOnce) @callee_owned () -> () {
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s19called_exactly_once15makeCapturelessyyXOyFyyXOfU_ : $@convention(thin) () -> ()
// CHECK:   [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) () -> () to $@called(exactlyOnce) @callee_owned () -> ()
// CHECK:   return [[THICK]] : $@called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once15makeCapturelessyyXOyF'
func makeCaptureless() -> @called(exactlyOnce) () -> Void {
  return { }
}

// A closure that captures a `@called(exactlyOnce)` value consumes it.
// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once13makeCapturingyyyXOyyXOnF : $@convention(thin) (@owned @called(exactlyOnce) @callee_owned () -> ()) -> @owned @called(exactlyOnce) @callee_owned () -> () {
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s19called_exactly_once13makeCapturingyyyXOyyXOnFyyXOfU_ : $@convention(thin) (@owned @called(exactlyOnce) @callee_owned () -> ()) -> ()
// CHECK:   [[CAPTURE:%.*]] = load [take] {{%.*}} : $*@called(exactlyOnce) @callee_owned () -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called(exactlyOnce)] [[CLOSURE]]([[CAPTURE]]) : $@convention(thin) (@owned @called(exactlyOnce) @callee_owned () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once13makeCapturingyyyXOyyXOnF'

// CHECK-LABEL: sil private [ossa] @$s19called_exactly_once13makeCapturingyyyXOyyXOnFyyXOfU_ : $@convention(thin) (@owned @called(exactlyOnce) @callee_owned () -> ()) -> () {
// CHECK: bb0({{%.*}} : @closureCapture @owned $@called(exactlyOnce) @callee_owned () -> ()):
// CHECK: } // end sil function '$s19called_exactly_once13makeCapturingyyyXOyyXOnFyyXOfU_'
func makeCapturing(_ f: @escaping @called(exactlyOnce) () -> Void) -> @called(exactlyOnce) () -> Void {
  return { @called(exactlyOnce) in f() }
}

// Converting a plain function to `@called(exactlyOnce)` goes through a thunk.
// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once13makeFromPlainyyyXOyycF : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> @owned @called(exactlyOnce) @callee_owned () -> () {
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeg_IeOx_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called(exactlyOnce)] [[THUNK]]({{%.*}}) : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once13makeFromPlainyyyXOyycF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeg_IeOx_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[FN:%.*]] : @guaranteed $@callee_guaranteed () -> ()):
// CHECK:   apply [[FN]]() : $@callee_guaranteed () -> ()
// CHECK: } // end sil function '$sIeg_IeOx_TR'
func makeFromPlain(_ f: @escaping () -> Void) -> @called(exactlyOnce) () -> Void {
  return f
}

// Converting a `@called(atMostOnce)` function to `@called(exactlyOnce)` also
// goes through a thunk, even though the two have the same ABI, so that every
// `@called(exactlyOnce)` value has a context of its own. The thunk consumes
// the source value when it calls it.
// CHECK-LABEL: sil hidden [ossa] @$s19called_exactly_once18makeFromAtMostOnceyyyXOyyXOonF : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> @owned @called(exactlyOnce) @callee_owned () -> () {
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeOox_IeOx_TR : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> ()
// CHECK:   [[RESULT:%.*]] = partial_apply [called(exactlyOnce)] [[THUNK]]({{%.*}}) : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> ()
// CHECK:   return [[RESULT]] : $@called(exactlyOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$s19called_exactly_once18makeFromAtMostOnceyyyXOyyXOonF'

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIeOox_IeOx_TR : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> () {
// CHECK: bb0([[FN:%.*]] : @owned $@called(atMostOnce) @callee_owned () -> ()):
// CHECK:   apply [[FN]]() : $@called(atMostOnce) @callee_owned () -> ()
// CHECK: } // end sil function '$sIeOox_IeOx_TR'
func makeFromAtMostOnce(_ f: @escaping @called(atMostOnce) () -> Void) -> @called(exactlyOnce) () -> Void {
  return f
}

// A witness with a `@called(exactlyOnce)` parameter can satisfy a
// `@called(atMostOnce)` requirement. The witness thunk converts the argument
// through a thunk, so that the witness gets a context of its own.
protocol RunsAtMostOnce {
  func run(_ f: @escaping @called(atMostOnce) () -> Void)
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s19called_exactly_once6RunnerVAA14RunsAtMostOnceA2aDP3runyyyyXOonFTW : $@convention(witness_method: RunsAtMostOnce) (@owned @called(atMostOnce) @callee_owned () -> (), @in_guaranteed Runner) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@called(atMostOnce) @callee_owned () -> (), {{%.*}} : $*Runner):
// CHECK:   [[THUNK:%.*]] = function_ref @$sIeOox_IeOx_TR : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> ()
// CHECK:   [[CONVERTED:%.*]] = partial_apply [called(exactlyOnce)] [[THUNK]]([[F]]) : $@convention(thin) (@owned @called(atMostOnce) @callee_owned () -> ()) -> ()
// CHECK:   [[WITNESS:%.*]] = function_ref @$s19called_exactly_once6RunnerV3runyyyyXOnF : $@convention(method) (@owned @called(exactlyOnce) @callee_owned () -> (), Runner) -> ()
// CHECK:   apply [[WITNESS]]([[CONVERTED]], {{%.*}})
// CHECK: } // end sil function '$s19called_exactly_once6RunnerVAA14RunsAtMostOnceA2aDP3runyyyyXOonFTW'
struct Runner: RunsAtMostOnce {
  func run(_ f: @escaping @called(exactlyOnce) () -> Void) { f() }
}
