// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: OS=macosx
// REQUIRES: asserts

func f(c1: [String], c2: [String], c3: [String], c4: [String], c5: [String], c6: [String], c7: [String], c8: [String], c9: [String], c10: [String]) {
  _ = c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10
}
