// RUN: %target-typecheck-verify-swift
extension Bool: @retroactive ExpressibleByIntegerLiteral {
	public init(integerLiteral value: Int) {
		self = value != 0
	}
}

enum IsDefinitelyRecursive : Bool, Equatable, Hashable {
  case recursive=false
}

// expected-error@+2 {{'IsRecursive' declares raw type 'Bool', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-note@+1 {{add stubs for conformance}}
enum IsRecursive : Bool, Equatable, Hashable {
  case recursive=false
  case nonrecursive // expected-error{{enum case must declare a raw value when the preceding raw value is not an integer}}
}

enum IsRecursiveBad1Integral : Bool, Equatable, Hashable {
  case recursive = 0
  case nonrecursive
}

// expected-error@+2 {{'IsRecursiveBad2' declares raw type 'Int', but does not conform to RawRepresentable and conformance could not be synthesized}}
// expected-note@+1 {{add stubs for conformance}}
enum IsRecursiveBad2 : Int, Equatable, Hashable {
  case recursive = false // expected-error{{cannot convert value of type 'Bool' to raw type 'Int'}}
}
