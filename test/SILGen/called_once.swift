// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute


// CHECK-LABEL: sil hidden [ossa] @$s11called_once12testCallOnceyyyyXOnF : $@convention(thin) (@owned @called(once) @callee_owned () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @owned $@called(once) @callee_owned () -> ()):
// CHECK:   [[LOCAL:%.*]] = alloc_box ${ let @called(once) @callee_owned () -> () }, let, name "local"
// CHECK: } // end sil function '$s11called_once12testCallOnceyyyyXOnF'
func testCallOnce(_ f: @called(once) () -> Void) {
  let local: @called(once) () -> Void = {}
  _ = local
}

func run() {
  // CHECK-LABEL: sil private [ossa] @$s11called_once3runyyFyyXOfU_ : $@convention(thin) () -> ()
  testCallOnce { }
}

// FIXME: The capture shouldn't be `@guaranteed` here
//
// CHECK-LABEL: sil hidden [ossa] @$s11called_once22testClosureWithCapture1fyyyXOn_tF : $@convention(thin) (@owned @called(once) @callee_owned () -> ()) -> () {
// CHECK:  [[FN:%.*]] = begin_borrow [lexical] [var_decl] %1 : ${ var @called(once) @callee_owned () -> () }
// CHECK:  [[CLOSURE_REF:%.*]] = function_ref @$s11called_once22testClosureWithCapture1fyyyXOn_tFyyXOfU_ : $@convention(thin) (@guaranteed { var @called(once) @callee_owned () -> () }) -> ()
// CHECK:  [[FN_COPY:%.*]] = copy_value [[FN]] : ${ var @called(once) @callee_owned () -> () }
// CHECK:  [[CLOSURE_WITH_CAPTURE:%.*]] = partial_apply [[CLOSURE_REF]]([[FN_COPY]]) : $@convention(thin) (@guaranteed { var @called(once) @callee_owned () -> () }) -> ()
// CHECK:  convert_function [[CLOSURE_WITH_CAPTURE]] : $@callee_owned () -> () to $@called(once) @callee_owned () -> ()
// CHECK: } // end sil function '$s11called_once22testClosureWithCapture1fyyyXOn_tF'
func testClosureWithCapture(f: @called(once) () -> Void) {
  let g = { @called(once) in f() }
  _ = g
}
