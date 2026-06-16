// Verify that importing a module where an unrelated nested class has its own
// operator== does not produce a cycle warning during iterator conformance
// derivation.

// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++17
// expected-no-diagnostics

import IteratorCycleUnrelatedOperator

typealias X = llvm.R
