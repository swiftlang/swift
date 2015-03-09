// RUN: %target-swift-frontend %s -O -emit-sil | not FileCheck %s

// FIXME: Existential devirtualization needs to be updated to work with
// open_existential_addr instructions. rdar://problem/18506660

protocol Pingable {
 func ping(x : Int);
}
class Foo : Pingable {
  func ping(x : Int) { var t : Int }
}

// Everything gets devirtualized, inlined, and promoted to the stack.
//CHECK: @_TF24devirtualize_existential17interesting_stuffFT_T_
//CHECK-NOT: init_existential_addr
//CHECK-NOT: apply
//CHECK: return
func interesting_stuff() {
 var x : Pingable = Foo()
 x.ping(1)
}

