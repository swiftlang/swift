// RUN: %target-swift-frontend %s -O -emit-sil | %FileCheck %s

protocol Pingable {
 func ping(_ x : Int);
}
class Foo : Pingable {
  func ping(_ x : Int) { var t : Int }
}

// Everything gets devirtualized, inlined, and promoted to the stack.
//CHECK: @_TF24devirtualize_existential17interesting_stuffFT_T_
//CHECK-NOT: init_existential_addr
//CHECK-NOT: apply
//CHECK: return
public func interesting_stuff() {
 var x : Pingable = Foo()
 x.ping(1)
}

