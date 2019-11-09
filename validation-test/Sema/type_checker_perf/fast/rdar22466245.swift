// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release

_ = (1...10).map(String.init) + [": hi"]
