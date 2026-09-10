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

// expected-note@+1 {{'Bad' is annotated unsafe in C++}}
struct HasBadField {
  Bad bad;
};

// expected-note@+1 {{'Bad' is annotated unsafe in C++}}
struct HasBadBase : Bad {};

// A non-escapable type is a safe "view" only if what it points to is
// self-contained. A 'void *' could point at anything, so this one is not.
// expected-note@+1 {{this is a non-escapable view whose lifetime dependency Swift cannot track}}
struct SWIFT_NONESCAPABLE Indirect {
  void *p;
};

// When escapability is unknown and a reason was recorded for it, that is
// reported instead: it is the root cause, and it is what the user can annotate.
// expected-note@+1 {{this type has unknown escapability: its member 'p' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct HasPointerField {
  int *p;
};

// An unsafe annotation settles escapability without recording a reason for it --
// the annotation already says everything -- so here the safety walk's own
// reasons are what surface. The pointer field is found before the annotated
// member type is popped, so it is the one named.
// expected-note@+1 {{this type has an unsafe field 'p'}}
struct MixedField {
  Bad bad;
  int *p;
};

// A base is only ever unsafe transitively, so the pointer field is named here
// too, not the base.
// expected-note@+1 {{this type has an unsafe field 'p'}}
struct MixedBase : Bad {
  int *p;
};

// Template arguments are checked before fields.
// expected-note@+1 {{this type has an unsafe template argument}}
template <class T, class U> struct Pair { T a; U b; };
using UnsafePair = Pair<int *, Bad>;

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

func useIndirectView(_ x: Indirect) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'Indirect'}}
}

func usePointerField(_ x: HasPointerField) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'HasPointerField'}}
}

func useMixedField(_ x: MixedField) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'MixedField'}}
}

func useMixedBase(_ x: MixedBase) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'MixedBase'}}
}

func useUnsafePair(_ x: UnsafePair) {
  _ = x // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'UnsafePair' (aka 'Pair<UnsafeMutablePointer<CInt>, Bad>')}}
}
