// Ensure that -switch-checking-invocation-threshold= frontend option works,
// stopping the check, producing the correct diagnostic, and not producing the
// diagnostic for a completed check.
//
// RUN: %empty-directory(%t)
//
// RUN: not %target-swift-frontend -typecheck %s -switch-checking-invocation-threshold=1 2>%t/unproven.txt
// RUN: %FileCheck %s --check-prefix UNABLE-TO-CHECK <%t/unproven.txt
// RUN: not %FileCheck %s --check-prefix MUST-BE-EXHAUSTIVE <%t/unproven.txt
//
// RUN: not %target-swift-frontend -typecheck %s  2>%t/disproved.txt
// RUN: %FileCheck %s --check-prefix MUST-BE-EXHAUSTIVE <%t/disproved.txt
// RUN: not %FileCheck %s --check-prefix UNABLE-TO-CHECK <%t/disproved.txt
//
// UNABLE-TO-CHECK: error: the compiler is unable to check that this switch is exhaustive in reasonable time
// MUST-BE-EXHAUSTIVE: error: switch must be exhaustive


enum A {
  case a1, a2, a3, a4, a5
}
enum B {
  case b1, b2, b3, b4
}

func f(a: A, b: B) {
  switch (a, b) {
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
