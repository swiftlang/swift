// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func neverReturns() -> Never { fatalError() }

// CHECK-LABEL: sil hidden [ossa] @$s22bottom_type_conversion15neverToAnythingyyF
// CHECK: bb0:
func neverToAnything() {
  var x: Int = neverReturns()
  // CHECK: [[PX:%.*]] = project_box {{%.*}} : ${ var Int }, 0
  // CHECK: [[FUNCREF:%.*]] = function_ref @$s22bottom_type_conversion12neverReturnss5NeverOyF : $@convention(thin) () -> Never
  // CHECK: {{%.*}} = apply [[FUNCREF]]() : $@convention(thin) () -> Never
  // CHECK: store undef to [trivial] [[PX]] : $*Int

  // FIXME: ownership verification failure for nontrivial tuple elements
  //var y: (String, Int) = neverReturns()

  var z: (Int)->String = neverReturns()
  // CHECK: [[PZ:%.*]] = project_box {{%.*}} : ${ var @callee_guaranteed (Int) -> @owned String }, 0
  // CHECK: [[FUNCREF:%.*]] = function_ref @$s22bottom_type_conversion12neverReturnss5NeverOyF : $@convention(thin) () -> Never
  // CHECK: {{%.*}} = apply [[FUNCREF]]() : $@convention(thin) () -> Never
  // CHECK: store undef to [init] [[PZ]] : $*@callee_guaranteed (Int) -> @owned String
}
