// RUN: %swift -emit-silgen %s | FileCheck %s

protocol Runcible {
  func runce<A>(x:A)
}

// CHECK: sil @_T15generic_witness3fooUS_8Runcible__FT1xQ__T_ : $[thin] <B : Runcible> (x : B) -> () {

func foo<B:Runcible>(x:B) {
  // CHECK: [[METHOD:%.*]] = archetype_method $*B, #Runcible.runce!1 : $[cc(method)] <A> ((x : A), [byref] B) -> ()
  // CHECK: specialize [[METHOD]], $[cc(method), thin] ((x : Int64), [byref] B) -> (), A = Int
  x.runce(5)
}

// CHECK: sil @_T15generic_witness3barFT1xPS_8Runcible__T_ : $[thin] (x : Runcible) -> ()
func bar(x : Runcible) {
  // CHECK: [[BOX:%.*]] = alloc_box $Runcible 
  // CHECK: [[EXIST:%.*]] = project_existential [[BOX]]#1 : $*Runcible
  // CHECK: [[METHOD:%.*]] = protocol_method [[BOX]]#1 : $*Runcible, #Runcible.runce!1 : $[cc(method)] <A> ((x : A), Builtin.OpaquePointer) -> ()
  // CHECK: specialize [[METHOD]], $[cc(method), thin] ((x : Int64), Builtin.OpaquePointer) -> (), A = Int
  x.runce(5)
}