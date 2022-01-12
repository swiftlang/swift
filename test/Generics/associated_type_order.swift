// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P1 {
  associatedtype B
}

protocol P2 {
  associatedtype A
}

// Make sure that T.[P1:A] < T.[P2:B].

struct G<T : P1 & P2> where T.A == T.B {
  // CHECK-LABEL: sil hidden [ossa] @$s21associated_type_order1GV3fooyy1AAA2P2PQzF : $@convention(method) <T where T : P1, T : P2, T.A == T.B> (@in_guaranteed T.A, G<T>) -> () {
  func foo(_: T.A) {}
}

