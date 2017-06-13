// RUN: %target-typecheck-verify-swift -warn-long-expression-type-checking=1 -warn-long-function-bodies=1 %s
func foo<T>(_ x: T) -> T { return x }
func foo(_ x: Int) -> Int { return x }

func test(m: Double) -> Int {
  // expected-warning@-1 {{global function 'test(m:)' took}}
  return Int(foo(Float(m) / 20) * 20 - 2) + 10
  // expected-warning@-1 {{expression took}}
}
