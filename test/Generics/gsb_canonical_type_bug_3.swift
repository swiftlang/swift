// RUN: %target-swift-emit-silgen %s -requirement-machine=on | %FileCheck %s

// The GSB computes an incorrect canonical type for bar() which causes
// SILGen to assert.

public protocol P1 {
  associatedtype A
}

public protocol P2 {
  associatedtype A
}

public protocol P3 : P1, P2 {
  typealias B = A
}

public protocol P4 {}

func bar<T : P3 & P4, X>(x: T, y: T.B, _: X) {}

// CHECK-LABEL: sil hidden [ossa] @$s24gsb_canonical_type_bug_33bar1x1y_yx_1AAA2P1PQzq_tAA2P3RzAA2P4Rzr0_lF : $@convention(thin) <T, X where T : P3, T : P4> (@in_guaranteed T, @in_guaranteed T.A, @in_guaranteed X) -> ()
