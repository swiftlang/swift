// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func rdar31742586() -> Double {
  return -(1 + 2) + -(3 + 4) + 5 - (-(1 + 2) + -(3 + 4) + 5)
}
