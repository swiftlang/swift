// RUN: %target-typecheck-verify-swift -solver-memory-threshold=1

func foo() { _ = 1 } // expected-error {{the compiler is unable to type-check this expression in reasonable time; try breaking up the expression into distinct sub-expressions}}
