// RUN: %target-typecheck-verify-swift -swift-version 4 -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func test(_ i: Int, _ j: Int) -> Int {
  return 1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40 +
         1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40 +
         1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40
}
