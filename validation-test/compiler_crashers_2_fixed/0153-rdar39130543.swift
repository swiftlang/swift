// RUN: %target-swift-frontend %s -emit-ir


protocol P20 { }

protocol P21b {
	associatedtype T

	func foo(_: (T?) -> Void)
}

protocol P21a {
	associatedtype T

	func bar(_: ([T]) -> Void)
}

extension P21b where Self: P21a, T: P20 {
	func foo(_: (T?) -> Void) {
		bar { _ in }
	}
}
