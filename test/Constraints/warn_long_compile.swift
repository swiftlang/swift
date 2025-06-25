// RUN: %target-typecheck-verify-swift -warn-long-expression-type-checking=1 -solver-disable-shrink -warn-long-function-bodies=1
@_silgen_name("generic_foo")
func foo<T>(_ x: T) -> T

@_silgen_name("foo_of_int")
func foo(_ x: Int) -> Int

func test(m: Double) -> Int {
  // expected-warning@-1 {{global function 'test(m:)' took}}
  return ~(~(~(~(~(~(~(~(~(~(~(~(0))))))))))))
  // expected-warning@-1 {{expression took}}
}
