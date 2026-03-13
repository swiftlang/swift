// RUN: %target-typecheck-verify-swift

protocol P1<A> {
	associatedtype A
}

protocol P2<A, B>: P1 {
	associatedtype B
}

protocol P3s {
	associatedtype A
	associatedtype B

	typealias SelfP2 = P2<B, B>
}
