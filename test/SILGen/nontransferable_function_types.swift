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
// CHECK:   convert_function {{%.*}} : $@callee_guaranteed () -> () to $@nontransferable @callee_guaranteed () -> ()
// CHECK-NOT: function_ref @$s{{.*}}TR
// CHECK: } // end sil function '$s{{.*}}14testConversion2fnyyyc_tF'
func testConversion(fn: @escaping () -> Void) {
  takesNontransferable(fn)
}
