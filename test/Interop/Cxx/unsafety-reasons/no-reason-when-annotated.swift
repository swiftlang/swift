// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety

// An annotation written in the header speaks for itself, so no reason note is
// added: a heuristic must not explain a decision it did not make.
//
// There is deliberately no -verify-additional-file here. Any note landing in
// ann.h is unexpected and fails the test, which is the assertion.

//--- Inputs/module.modulemap
module Ann {
    header "ann.h"
    requires cplusplus
}

//--- Inputs/ann.h
#include "swift/bridging"

struct SWIFT_UNSAFE Annotated {
  int x;
};

// Unsafe for a reason the heuristics could explain -- a pointer member -- but
// the annotation is the decision that was taken, so it is left to speak for
// itself.
struct SWIFT_UNSAFE AnnotatedWithPointer {
  int *numbers;
};

//--- test.swift
import Ann

func useAnnotatedType(_ x: Annotated) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'Annotated'}}
}

func useAnnotatedWithPointer(_ x: AnnotatedWithPointer) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'AnnotatedWithPointer'}}
}
