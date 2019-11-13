// RUN: %target-typecheck-verify-swift -swift-version 5 -solver-enable-operator-designated-types -solver-disable-shrink -disable-constraint-solver-performance-hacks

func test(_ x: Int) -> Int {
  return x + nil
  // expected-error@-1 {{'nil' is not compatible with expected argument type 'Int'}}
}
