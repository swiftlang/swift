// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// https://github.com/swiftlang/swift/issues/53523

// Invalid expression, there is no (Double, Int) overload of /

public func slow(d: Double, n: Int) {
  return d * 1.0 + 1.0 / n + d / d
  // expected-error@-1 {{reasonable time}}
}
