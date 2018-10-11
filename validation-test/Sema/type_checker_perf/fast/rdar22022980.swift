// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 5 -swift-version 5 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

_ = [1, 3, 5, 7, 11].filter{ $0 == 1 || $0 == 3 || $0 == 11 || $0 == 1 || $0 == 3 || $0 == 11 } == [ 1, 3, 11 ]
