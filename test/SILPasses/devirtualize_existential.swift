// RUN: %swift %s -O3 -emit-sil | FileCheck %s

protocol Pingable {
 func ping(x : Int);
}
class Foo : Pingable {
  func ping(x : Int) { var t : Int }
}

//CHECK: @_TF24devirtualize_existential17interesting_stuffFT_T_
//CHECK: init_existential
//CHECK: function_ref @_TTWC24devirtualize_existential3FooS_8PingableS_FS1_4pingU_fRQPS1_FT1xSi_T_
//CHECK: apply
//CHECK: return
func interesting_stuff() {
 var x : Pingable = Foo()
 x.ping(1)
}

