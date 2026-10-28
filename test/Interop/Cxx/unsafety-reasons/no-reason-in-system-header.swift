// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -Xcc -isystem -Xcc %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety

// A note in a system header names something the user cannot annotate, and it
// would be unmatchable in a -verify test besides. The unsafe-use warning still
// fires; only the explanation is dropped.
//
// There is deliberately no -verify-additional-file here. Any note landing in
// sys.h is unexpected and fails the test, which is the assertion.

//--- Inputs/module.modulemap
module Sys {
    header "sys.h"
    requires cplusplus
}

//--- Inputs/sys.h
#include "swift/bridging"

struct Owner {
  void *ptr;
  Owner(const Owner &);

  int *data() const;
};

struct HasPointerField {
  int *p;
};

//--- test.swift
import Sys

func useProjection(_ o: Owner) {
  _ = o.__dataUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__dataUnsafe()'}}
  // expected-note@-2 {{reference to parameter 'o' involves unsafe type 'Owner'}}
}

func useRecord(_ x: HasPointerField) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'HasPointerField'}}
}
