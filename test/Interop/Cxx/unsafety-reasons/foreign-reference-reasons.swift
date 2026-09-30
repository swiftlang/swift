// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}frt.h

// A type imported as a class must be explained under the rules it was imported
// with: unsafety comes from its bases and from nothing else. Asking under the
// record rules answers that it is safe, and the note goes missing.
//
// A call to a virtual method goes through a synthesized thunk. The note belongs
// on the method the user wrote, not on the thunk.

//--- Inputs/module.modulemap
module FRT {
    header "frt.h"
    requires cplusplus
}

//--- Inputs/frt.h
#include "swift/bridging"

struct SWIFT_NONESCAPABLE View {
  const int *p;
};

struct SWIFT_UNSAFE BadBase {
  int x;
};

// Unsafe through its base. The pointer field is not what makes it unsafe: under
// the class rules a field never does.
// expected-note@+1 {{'BadBase' is annotated unsafe in C++}}
struct UnsafeRef : BadBase {
  int *buffer;
} SWIFT_SHARED_REFERENCE(retainUnsafeRef, releaseUnsafeRef);

inline void retainUnsafeRef(UnsafeRef *) {}
inline void releaseUnsafeRef(UnsafeRef *) {}

struct Ref {
  // expected-note@+1 {{non-escapable parameter 'v' has no lifetime annotation}}
  void takeView(View v) const;
  // expected-note@+1 {{non-escapable parameter 'v' has no lifetime annotation}}
  virtual void virtualTakeView(View v) const;
} SWIFT_SHARED_REFERENCE(retainRef, releaseRef);

inline void retainRef(Ref *) {}
inline void releaseRef(Ref *) {}

//--- test.swift
import FRT

func useUnsafeRef(_ r: UnsafeRef) {
  _ = r // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'r' involves unsafe type 'UnsafeRef'}}
}

func callMethod(_ r: Ref, _ v: View) {
  r.takeView(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'takeView'}}
}

// The note names the method, not the synthesized thunk that dispatches to it.
func callVirtualMethod(_ r: Ref, _ v: View) {
  r.virtualTakeView(v) // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to unsafe instance method 'virtualTakeView'}}
}
