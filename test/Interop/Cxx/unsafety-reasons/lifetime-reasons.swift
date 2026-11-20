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

struct Captured {
  int data;
  // expected-note@+1 {{Swift cannot represent a parameter capturing 'self' in an initializer}}
  Captured(View &out) [[clang::lifetime_capture_by(out)]];
};

// An independent result does not hide a capture Swift cannot represent.
// expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'o', which may be captured by a global or unknown entity}}
View independentCaptureGlobally(const Owner &o [[clang::lifetime_capture_by_global]]) SWIFT_RETURNS_INDEPENDENT_VALUE;

// An unannotated non-Escapable parameter does not make an API with an
// independent result unsafe.
View independentOfView(View v) SWIFT_RETURNS_INDEPENDENT_VALUE;

struct Shared {
  int data;
} SWIFT_IMMORTAL_REFERENCE;

// Capturing into the constructed object has the limits of 'lifetimebound'.
struct SWIFT_NONESCAPABLE CapturingCtor {
  // expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 'v', an rvalue reference that is not guaranteed to outlive the call}}
  CapturingCtor(View &&v [[clang::lifetime_capture_by_this]]);
  // expected-note@+1 {{Swift cannot represent the lifetime annotation on parameter 's', which is imported as a class}}
  CapturingCtor(const Shared &s [[clang::lifetime_capture_by_this]]);
  const int *p;
};

// Swift drops a dependency whose target is Escapable, so capturing into one is
// not enforced.
struct SWIFT_ESCAPABLE EscapableSink {
  const int *p;
  // expected-note@+1 {{this lifetime annotation is ignored: parameter 'o' is captured by 'self', which is Escapable, so Swift drops the dependency}}
  void add(const Owner &o [[clang::lifetime_capture_by_this]]);
  // expected-note@+1 {{this lifetime annotation is ignored: parameter 'v' is captured by 'self', which is Escapable, so Swift drops the dependency}}
  void addView(View v [[clang::lifetime_capture_by_this]]);
};

struct SWIFT_NONESCAPABLE ViewSource {
  const int *p;
  // expected-note@+1 {{this lifetime annotation is ignored: 'self' is captured by parameter 'out', which is Escapable, so Swift drops the dependency}}
  void handOut(EscapableSink &out) const [[clang::lifetime_capture_by(out)]];
};

// expected-note@+1 {{this lifetime annotation is ignored: parameter 'o' is captured by parameter 'out', which is Escapable, so Swift drops the dependency}}
void captureIntoEscapable(EscapableSink &out, const Owner &o [[clang::lifetime_capture_by(out)]]);
// expected-note@+1 {{this lifetime annotation is ignored: parameter 'v' is captured by parameter 'out', which is Escapable, so Swift drops the dependency}}
void captureViewIntoEscapable(EscapableSink &out, View v [[clang::lifetime_capture_by(out)]]);

// Constructing an Escapable value drops its dependencies too.
struct SWIFT_ESCAPABLE EscapableBox {
  const int *p;
  // expected-warning@+2 {{the returned type 'EscapableBox' is annotated as escapable; it cannot have lifetime dependencies}}
  // expected-note@+1 {{this lifetime annotation is ignored: the result is Escapable, so Swift drops the dependency}}
  EscapableBox(const Owner &o [[clang::lifetime_capture_by_this]]);
  // expected-warning@+2 {{the returned type 'EscapableBox' is annotated as escapable; it cannot have lifetime dependencies}}
  // expected-note@+1 {{this lifetime annotation is ignored: the result is Escapable, so Swift drops the dependency}}
  EscapableBox(const View &v [[clang::lifetimebound]]);
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

func capturedByConstructor(_ v: View) {
  var out = v
  _ = Captured(&out) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe initializer 'init(_:)'}}
}

func independentResults(_ o: Owner, _ v: View) {
  _ = independentCaptureGlobally(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'independentCaptureGlobally'}}
  _ = independentOfView(v)
}

func capturedIntoConstructed(_ v: consuming View, _ s: Shared) {
  _ = CapturingCtor(consuming: v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe initializer 'init(consuming:)'}}
  _ = CapturingCtor(s) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe initializer 'init(_:)'}}
}

func capturedIntoEscapable(_ o: Owner, _ v: View, _ s: EscapableSink, _ src: ViewSource) {
  var sink = s
  sink.add(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'add'}}
  sink.addView(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'addView'}}
  src.handOut(&sink) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'handOut'}}
  captureIntoEscapable(&sink, o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'captureIntoEscapable'}}
  captureViewIntoEscapable(&sink, v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe global function 'captureViewIntoEscapable'}}
}

func constructedEscapable(_ o: Owner, _ v: View) {
  _ = EscapableBox(o) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe initializer 'init(_:)'}}
  _ = EscapableBox(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe initializer 'init(_:)'}}
}
