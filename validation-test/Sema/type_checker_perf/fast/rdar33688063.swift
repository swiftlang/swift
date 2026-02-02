// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200
// REQUIRES: tools-release,no_asan

let _ = 1 | UInt32(0) << 0 | UInt32(1) << 1 | UInt32(2) << 2 | UInt32(3) << 3 | UInt32(4) << 4
