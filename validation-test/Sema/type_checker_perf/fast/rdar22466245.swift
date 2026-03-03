// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000
// REQUIRES: tools-release,no_asan

_ = (1...10).map(String.init) + [": hi"]
