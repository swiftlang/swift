// RUN: %swift -emit-silgen %s | FileCheck %s

protocol Runcible {
  func runce<A>(x: A)
}

// CHECK-LABEL: sil  @_TF15generic_witness3fooUS_8Runcible__FQ_T_ : $@thin <B where B : Runcible> (@in B) -> () {

func foo<B : Runcible>(x: B) {
  // CHECK: [[METHOD:%.*]] = witness_method $B, #Runcible.runce!1 : $@cc(witness_method) @thin <τ_0_0, τ_1_0 where τ_0_0 : Runcible> (@in τ_1_0, @inout τ_0_0) -> ()
  // CHECK: apply [[METHOD]]<B, Int>
  x.runce(5)
}

// CHECK-LABEL: sil  @_TF15generic_witness3bar{{.*}} : $@thin (@in Runcible) -> ()
func bar(var x: Runcible) {
  // CHECK: [[BOX:%.*]] = alloc_box $Runcible 
  // CHECK: [[EXIST:%.*]] = project_existential [[BOX]]#1 : $*Runcible
  // CHECK: [[METHOD:%.*]] = protocol_method [[BOX]]#1 : $*Runcible, #Runcible.runce!1 : <`Self` : Runcible> inout Self -> <A> (A) -> (), $@cc(witness_method) @callee_owned <τ_1_0> (@in τ_1_0, @inout Self) -> ()
  // CHECK: apply [[METHOD]]<Int>
  x.runce(5)
}
