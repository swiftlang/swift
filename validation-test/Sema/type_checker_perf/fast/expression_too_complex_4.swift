// RUN: %target-typecheck-verify-swift -swift-version 4 -solver-expression-time-threshold=1 -swift-version 5 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func test(_ i: Int, _ j: Int) -> Int {
  return 1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40 +
         1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40 +
         1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40
}
