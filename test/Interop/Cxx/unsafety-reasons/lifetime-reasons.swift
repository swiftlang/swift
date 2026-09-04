// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}life.h

// Lifetime inference records why it made a declaration unsafe at the point it
// decides, because the conditions depend on state that is gone by the time a
// diagnostic is emitted.

//--- Inputs/module.modulemap
module Life {
    header "life.h"
    requires cplusplus
}

//--- Inputs/life.h
#include "swift/bridging"

struct SWIFT_NONESCAPABLE View {
  const int *p;
};

// A non-escapable parameter with no lifetime annotation: the note names it.
// expected-note@+1 {{'takeView' is unsafe because its non-escapable parameter 'v' has no lifetime annotation}}
void takeView(View v);

// Annotating the parameter as non-escaping settles it, so this one is safe and
// keeps the test honest about -strict-memory-safety being in effect.
void takeViewNoEscape(View v [[clang::noescape]]);

struct Owner {
  int data;
};

// A written annotation Swift cannot represent: the note names the parameter and
// what defeated the annotation.
// expected-note@+1 {{'viewFromRValue' is unsafe because Swift cannot represent the lifetime annotation on parameter 'o', an rvalue reference that is not guaranteed to outlive the call}}
View viewFromRValue(Owner &&o [[clang::lifetimebound]]);

// expected-note@+1 {{'viewFromValue' is unsafe because Swift cannot represent the lifetime annotation on parameter 'o', which has no borrowable storage}}
View viewFromValue(Owner o [[clang::lifetimebound]]);

//--- test.swift
import Life

func unannotated(_ v: View) {
  takeView(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'takeView'}}
}

func annotated(_ v: View) {
  takeViewNoEscape(v)
}

func skippedAnnotations(_ o: Owner) {
  _ = viewFromRValue(consuming: o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'viewFromRValue(consuming:)'}}
  _ = viewFromValue(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'viewFromValue'}}
}
