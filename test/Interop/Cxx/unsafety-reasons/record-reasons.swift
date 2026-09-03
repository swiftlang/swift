// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}rec.h

// A record is unsafe because of something it contains. The walk is transitive,
// so the note names the root cause rather than the immediate field or base.

//--- Inputs/module.modulemap
module Rec {
    header "rec.h"
    requires cplusplus
}

//--- Inputs/rec.h
#include "swift/bridging"

struct SWIFT_UNSAFE Bad {
  int x;
};

// expected-note@+1 {{type 'HasBadField' is unsafe because 'Bad' is annotated unsafe in C++}}
struct HasBadField {
  Bad bad;
};

// expected-note@+1 {{type 'HasBadBase' is unsafe because 'Bad' is annotated unsafe in C++}}
struct HasBadBase : Bad {};

//--- test.swift
import Rec

func useField(_ x: HasBadField) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'HasBadField'}}
}

func useBase(_ x: HasBadBase) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'HasBadBase'}}
}
