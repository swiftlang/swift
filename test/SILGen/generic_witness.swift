// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Runcible {
  func runce<A>(x: A)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3fooUS_8Runcible__FQ_T_ : $@thin <B where B : Runcible> (@in B) -> () {

func foo<B : Runcible>(x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : Runcible><τ_1_0> (@in τ_1_0, @in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil hidden @_TF15generic_witness3bar{{.*}} : $@thin (@in Runcible) -> ()
func bar(var x: Runcible) {
  // CHECK: [[BOX:%.*]] = alloc_box $Runcible 
  // CHECK: [[EXIST:%.*]] = open_existential [[BOX]]#1 : $*Runcible to $*[[OPENED:@opened(.*) Runcible]]
  // CHECK: [[METHOD:%.*]] = witness_method $[[OPENED]], #Runcible.runce!1
  // CHECK: apply [[METHOD]]<[[OPENED]], Int>
  x.runce(5)
}
