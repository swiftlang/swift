// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Runcible {
  func runce<A>(_ x: A)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3foo{{.*}} : $@convention(thin) <B where B : Runcible> (@in B) -> () {

func foo<B : Runcible>(_ x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : Runcible><τ_1_0> (@in τ_1_0, @in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3bar{{.*}} : $@convention(thin) (@in Runcible) -> ()
func bar(_ x: Runcible) {
  var x = x
  // CHECK: [[BOX:%.*]] = alloc_box $Runcible
  // CHECK: [[TEMP:%.*]] = alloc_stack $Runcible
  // CHECK: [[EXIST:%.*]] = open_existential_addr [[TEMP]] : $*Runcible to $*[[OPENED:@opened(.*) Runcible]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #Runcible.runce!1
  // CHECK: apply [[METHOD]]<[[OPENED]], Int>
  x.runce(5)
}
