// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}proj.h

// Using an unsafe C++ projection should say why it is unsafe, at the C++
// declaration responsible.

//--- Inputs/module.modulemap
module Proj {
    header "proj.h"
    requires cplusplus
}

//--- Inputs/proj.h
#include "swift/bridging"

// The user-declared copy constructor makes Owner self-contained, so returning
// a pointer out of it is a projection.
// expected-note@+1 {{type 'Owner' is unsafe because its field 'ptr' is unsafe}}
struct Owner {
  void *ptr;
  Owner(const Owner &);

  // expected-note@+1 {{'data' is unsafe because it returns a pointer or reference into a type that owns its storage}}
  int *data() const;
};

//--- test.swift
import Proj

func use(_ o: Owner) {
  _ = unsafe o.__dataUnsafe()
  _ = o.__dataUnsafe() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method '__dataUnsafe()'}}
  // expected-note@-2 {{reference to parameter 'o' involves unsafe type 'Owner'}}
}
