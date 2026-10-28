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
// expected-note@+1 {{non-'Escapable' parameter 'v' has no lifetime annotation}}
void takeView(View v);

// Annotating the parameter as non-escaping settles it, so this one is safe and
// keeps the test honest about -strict-memory-safety being in effect.
void takeViewNoEscape(View v [[clang::noescape]]);

struct Owner {
  int data;
};

// A written annotation Swift cannot represent: the note names the parameter and
// what defeated the annotation.
// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', an rvalue reference that is not guaranteed to outlive the call}}
View viewFromRValue(Owner &&o [[clang::lifetimebound]]);

// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', which has no borrowable storage}}
View viewFromValue(Owner o [[clang::lifetimebound]]);

// Swift cannot express an escape to a global or unknown location.
// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', which may be captured by a global or unknown entity}}
void captureRefGlobally(const Owner &o [[clang::lifetime_capture_by_global]]);

// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'v', which may be captured by a global or unknown entity}}
void captureViewUnknown(View v [[clang::lifetime_capture_by_unknown]]);

// A representable capture next to it does not make the function safe.
// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'v', which may be captured by a global or unknown entity}}
void captureOutAndGlobally(View v [[clang::lifetime_capture_by(out)]] [[clang::lifetime_capture_by_global]], View &out);

// Even an escapable value passed by copy may carry a contract Swift does not
// see, e.g. a pointer it holds.
// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', which may be captured by a global or unknown entity}}
void copyGlobally(Owner o [[clang::lifetime_capture_by_global]]);

// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', which is Escapable and imported by value}}
void copyInto(Owner o [[clang::lifetime_capture_by(out)]], View &out);

// Capturing a reference is a borrow Swift can represent.
void captureRefInto(const Owner &o [[clang::lifetime_capture_by(out)]], View &out);

struct Stash {
  int data;
  // expected-note@+1 {{Swift cannot represent the lifetime annotation on 'self', which may be captured by a global or unknown entity}}
  void stash() const [[clang::lifetime_capture_by_global]];
  // expected-note@+1 {{Swift cannot represent the lifetime annotation on 'self', which is Escapable and imported by value}}
  void handOut(View &out) const [[clang::lifetime_capture_by(out)]];
};

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

func capturedGlobally(_ o: Owner, _ v: View, _ s: Stash) {
  var out = v
  captureRefGlobally(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'captureRefGlobally'}}
  captureViewUnknown(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'captureViewUnknown'}}
  captureOutAndGlobally(v, &out) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'captureOutAndGlobally'}}
  s.stash() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'stash()'}}
  copyGlobally(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'copyGlobally'}}
}

func capturedEscapable(_ o: Owner, _ v: View, _ s: Stash) {
  var out = v
  copyInto(o, &out) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'copyInto'}}
  s.handOut(&out) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'handOut'}}
  captureRefInto(o, &out)
}
