// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 4
// REQUIRES: tools-release,no_asserts

let _ = 1 | UInt32(0) << 0 | UInt32(1) << 1 | UInt32(2) << 2 | UInt32(3) << 3 | UInt32(4) << 4
// expected-error@-1 {{reasonable time}}
