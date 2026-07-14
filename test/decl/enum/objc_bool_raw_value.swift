// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

extension Bool: @retroactive ExpressibleByIntegerLiteral {
	public init(integerLiteral value: Int) {
		self = value != 0
	}
}

@objc enum IsDefinitelyRecursive : Bool, Equatable, Hashable { // expected-error{{'@objc' enum raw type 'Bool' must be an integer type expressible in Objective-C}}
  case recursive=false
}
