// RUN: %target-swift-frontend -typecheck %s
// RUN: %target-typecheck-verify-swift -DBREAKING_POINT

func wrap<T>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByIntegerLiteral>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByFloatLiteral>(_ key: String, _ value: T) -> T { return value }
func wrap<T: ExpressibleByStringLiteral>(_ key: String, _ value: T) -> T { return value }

func wrapped(i: Int) -> Int {
  return wrap("1", i) + wrap("1", i) + wrap("1", i) + wrap("1", i) + wrap("1", i) + wrap("1", i) + wrap("1", i)
}

#if BREAKING_POINT
func wrapped2(i: Int) -> Int {
  // expected-error@+1{{reasonable time}}
  return wrap("1", 0) + wrap("1", 0) + wrap("1", 0) + wrap("1", 0)
}
#endif
