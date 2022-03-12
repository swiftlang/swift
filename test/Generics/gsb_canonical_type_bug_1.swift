// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// The GSB computes an incorrect canonical type here, which caused a SILGen assert.

protocol P1 {
  associatedtype A
}

protocol P2 {
  associatedtype A
}

extension P1 where Self: P2 {
  func foo<T>(_: A, _: T) { }
}

// CHECK-LABEL: sil hidden [ossa] @$s24gsb_canonical_type_bug_12P1PA2A2P2RzrlE3fooyy1AACQz_qd__tlF : $@convention(method) <Self where Self : P1, Self : P2><T> (@in_guaranteed Self.A, @in_guaranteed T, @in_guaranteed Self) -> ()
