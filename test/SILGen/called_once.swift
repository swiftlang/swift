// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute


// CHECK-LABEL: sil hidden [ossa] @$s11called_once12testCallOnceyyyyXOnF : $@convention(thin) (@owned @called_once @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[F:%.*]] : @noImplicitCopy @owned $@called_once @callee_guaranteed () -> ()):
// CHECK:   [[LOCAL:%.*]] = alloc_box ${ let @called_once @callee_guaranteed () -> () }, let, name "local"
// CHECK: } // end sil function '$s11called_once12testCallOnceyyyyXOnF'
func testCallOnce(_ f: @called(once) () -> Void) {
  let local: @called(once) () -> Void = {}
  _ = local
}

func run() {
  // CHECK-LABEL: sil private [ossa] @$s11called_once3runyyFyyXOfU_ : $@convention(thin) () -> ()
  testCallOnce { }
}
