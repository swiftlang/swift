// Ensure that -switch-checking-invocation-threshold= frontend option works,
// stopping the check, producing the correct diagnostic, and not producing the
// diagnostic for a completed check.
//
// RUN: %target-typecheck-verify-swift -verify-tag=UNABLE-TO-CHECK -switch-checking-invocation-threshold=1
// RUN: %target-typecheck-verify-swift -verify-tag=MUST-BE-EXHAUSTIVE


enum A {
  case a1, a2, a3, a4, a5
}
enum B {
  case b1, b2, b3, b4
}

func f(a: A, b: B) {
  switch (a, b) {
    // expected-error(UNABLE-TO-CHECK)@-1 {{the compiler is unable to check that this switch is exhaustive in reasonable time}}
    // expected-note(UNABLE-TO-CHECK)@-2 {{do you want to add a default clause?}}
    // expected-error(MUST-BE-EXHAUSTIVE)@-3 {{switch must be exhaustive}}
    // expected-note(MUST-BE-EXHAUSTIVE)@-4 {{add missing case}}
    case
//    (.a1, .b1),
    (.a2, .b1),
    (.a3, .b1),
    (.a4, .b1),
    (.a5, .b1),
    (.a1, .b2),
    (.a2, .b2),
    (.a3, .b2),
    (.a4, .b2),
    (.a5, .b2),
    (.a1, .b3),
    (.a2, .b3),
    (.a3, .b3),
    (.a4, .b3),
    (.a5, .b3),
    (.a1, .b4),
    (.a2, .b4),
    (.a3, .b4),
    (.a4, .b4),
    (.a5, .b4):
    break
  }
}
