// RUN: %target-typecheck-verify-swift -swift-version 5 -solver-enable-operator-designated-types -solver-disable-shrink -disable-constraint-solver-performance-hacks

func test(_ x: Int) -> Int {
  return x + nil
  // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '_.Stride'}}
}
