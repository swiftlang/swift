// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func wrap<T>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByIntegerLiteral>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByFloatLiteral>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByStringLiteral>(_ key: String, _ value: T) -> T { return value }

func wrapped(i: Int) -> Int {
  // FIXME: When this stops being "too complex", turn the integer value into
  // an integer literal.
  // expected-error@+1{{reasonable time}}
  return wrap("1", i) + wrap("1", i) + wrap("1", i) + wrap("1", i)
}
