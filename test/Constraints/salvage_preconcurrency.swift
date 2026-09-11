// RUN: %target-typecheck-verify-swift

func testInOut(_ arr: inout [Any]) {}
func testInOut(_ dict: inout [String: Any]) {}

@preconcurrency var dict: [String : any Sendable] = ["a": 42]
@preconcurrency var arr: [any Sendable] = [42]

testInOut(&arr)
// expected-error@-1 {{failed to produce diagnostic for expression}}
testInOut(&dict)
// expected-error@-1 {{failed to produce diagnostic for expression}}
