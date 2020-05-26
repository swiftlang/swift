// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

extension Bool: ExpressibleByIntegerLiteral {
	public init(integerLiteral value: Int) {
		self = value != 0
	}
}

@objc enum IsDefinitelyRecursive : Bool, Equatable, Hashable { // expected-error{{'@objc' enum raw type 'Bool' is not an integer type}}
  case recursive=false
}
