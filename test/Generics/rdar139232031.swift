// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype A
  associatedtype B
}

protocol P2<A>: P1 where B: P2<A> {}
