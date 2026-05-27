// RUN: %target-typecheck-verify-swift -solver-scope-threshold=5000 -swift-version 5
// REQUIRES: tools-release,no_asan

_ = [1, 3, 5, 7, 11].filter{ $0 == 1 || $0 == 3 || $0 == 11 || $0 == 1 || $0 == 3 || $0 == 11 } == [ 1, 3, 11 ]
