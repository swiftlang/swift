// RUN: %target-typecheck-verify-swift

_ = a.init
_ = b.init

enum a : Int { case x = #/ /# }  // expected-error {{raw value for enum case must be a literal}}
enum b : String { case x = #file }  // expected-error {{raw value for enum case must be a literal}}

// A value the parser accepts can still stop being a literal during type
// checking, so the parser's screen is not the only one needed. Here the raw
// type is optional, and conversion wraps the literal in an injection.
extension Optional : @retroactive ExpressibleByIntegerLiteral {
  public init(integerLiteral value: Int) { self = nil }
}

_ = c.init

enum c : Character? { case x = "x" }  // expected-error {{raw value for enum case must be a literal}}
