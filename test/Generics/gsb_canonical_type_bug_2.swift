// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P1 {
  associatedtype A
}

protocol P2 {
  associatedtype A
}

protocol P3 {
  associatedtype B
  associatedtype C : P3 where C.C == C, C.B == B
}

extension P2 {
  func foo<T : P1 & P3>(_: T, _: T.A) where T.B == T, T.A == A {
    _ = A.self
    _ = T.A.self
  }
}

// CHECK-LABEL:     sil hidden [ossa] @$s24gsb_canonical_type_bug_22P2PAAE3fooyyqd___1AQztAA2P1Rd__AA2P3Rd__1BAaHPQyd__Rsd__AeaGPQyd__AFRSlF : $@convention(method) <Self where Self : P2><T where T : P1, T : P3, T == T.B, Self.A == T.A> (@in_guaranteed T, @in_guaranteed Self.A, @in_guaranteed Self) -> ()
