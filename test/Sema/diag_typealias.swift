// RUN: %target-typecheck-verify-swift

struct S {}

typealias S = S // expected-error {{redundant type alias declaration}}{{1-17=}}


struct HasStatic {
	func foo() {
		print(cvar) // expected-error {{static element cvar cannot be referenced as an instance member}}
	}

	static let cvar = 123
}