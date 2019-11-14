// RUN: %target-typecheck-verify-swift
extension Bool: ExpressibleByIntegerLiteral {
	public init(integerLiteral value: Int) {
		self = value != 0
	}
}

enum IsDefinitelyRecursive : Bool, Equatable, Hashable {
  case recursive=false
}

// expected-error@+1{{'IsRecursive' declares raw type 'Bool', but does not conform to RawRepresentable and conformance could not be synthesized}}
enum IsRecursive : Bool, Equatable, Hashable {
  case recursive=false
  case nonrecursive // expected-error{{enum case must declare a raw value when the preceding raw value is not an integer}}
}
