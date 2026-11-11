// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -language-mode 6 -enable-experimental-feature NontransferableTypes %s | %FileCheck %s

// REQUIRES: swift_feature_NontransferableTypes

struct NonTransferable: ~Transferable {
  func use() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}20takesNontransferableyyyyYdcF : $@convention(thin) (@guaranteed @nontransferable @callee_guaranteed () -> ()) -> ()
func takesNontransferable(_: @escaping @nontransferable () -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11testClosure2ntyAA15NonTransferableV_tF :
// CHECK:   [[CLOSURE:%.*]] = function_ref @$s{{.*}}fU_ : $@convention(thin) @nontransferable
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE]]({{.*}}) : $@convention(thin) @nontransferable
// CHECK-NOT: convert_function
// CHECK:   apply {{%.*}}([[PA]]) : $@convention(thin) (@guaranteed @nontransferable @callee_guaranteed () -> ()) -> ()
// CHECK: } // end sil function '$s{{.*}}11testClosure2ntyAA15NonTransferableV_tF'
func testClosure(nt: NonTransferable) {
  takesNontransferable { nt.use() }
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}14testConversion2fnyyyc_tF :
// CHECK-NOT: convert_function {{%.*}} : $@callee_guaranteed () -> () to $@nontransferable
// CHECK:   [[THUNK:%.*]] = function_ref @$s{{.*}}TR : $@convention(thin) @nontransferable (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]({{%.*}}) : $@convention(thin) @nontransferable (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   apply {{%.*}}([[PA]]) : $@convention(thin) (@guaranteed @nontransferable @callee_guaranteed () -> ()) -> ()
// CHECK: } // end sil function '$s{{.*}}14testConversion2fnyyyc_tF'
func testConversion(fn: @escaping () -> Void) {
  takesNontransferable(fn)
}

func takesNoescapeNontransferable(_: @nontransferable () -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}12testNoescape2fnyyyc_tF :
// CHECK-NOT: convert_function {{%.*}} : $@callee_guaranteed () -> () to $@nontransferable
// CHECK:   [[THUNK:%.*]] = function_ref @$s{{.*}}TR : $@convention(thin) @nontransferable (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]({{%.*}})
// CHECK:   convert_escape_to_noescape [not_guaranteed] [[PA]] : $@nontransferable @callee_guaranteed () -> () to $@noescape @nontransferable @callee_guaranteed () -> ()
// CHECK: } // end sil function '$s{{.*}}12testNoescape2fnyyyc_tF'
func testNoescape(fn: @escaping () -> Void) {
  takesNoescapeNontransferable(fn)
}

func global() {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}8testThinyyF :
// CHECK-NOT: function_ref @$s{{.*}}TR
// CHECK:   thin_to_thick_function {{%.*}} : $@convention(thin) {{.*}}() -> () to $@nontransferable @callee_guaranteed () -> ()
// CHECK-NOT: function_ref @$s{{.*}}TR
// CHECK: } // end sil function '$s{{.*}}8testThinyyF'
func testThin() {
  takesNontransferable(global)
}

// FIXME: The partial application already forms a context of its own, so it shouldn't need a thunk but this is narrow enough case that
//        it might not actually matter in practice.
// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}9testLocal1xySi_tF :
// CHECK:   [[LOCAL:%.*]] = partial_apply [callee_guaranteed] {{%.*}}({{%.*}}) : $@convention(thin) (Int) -> ()
// CHECK:   [[THUNK:%.*]] = function_ref @$s{{.*}}TR : $@convention(thin) @nontransferable (@guaranteed @callee_guaranteed () -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[LOCAL]])
// CHECK: } // end sil function '$s{{.*}}9testLocal1xySi_tF'
func testLocal(x: Int) {
  func local() { _ = x }
  takesNontransferable(local)
}

func generic<T>(_: T) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}11testGenericyyF :
// CHECK:   [[GENERIC:%.*]] = partial_apply [callee_guaranteed] {{%.*}}<Int>() : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[THUNK:%.*]] = function_ref @$s{{.*}}TR : $@convention(thin) @nontransferable (Int, @guaranteed @callee_guaranteed (@in_guaranteed Int) -> ()) -> ()
// CHECK:   partial_apply [callee_guaranteed] [[THUNK]]([[GENERIC]])
// CHECK: } // end sil function '$s{{.*}}11testGenericyyF'
func testGeneric() {
  let _: @nontransferable (Int) -> Void = generic
}
