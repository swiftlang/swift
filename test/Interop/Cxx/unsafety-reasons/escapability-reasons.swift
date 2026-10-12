// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -iapinotes-modules -Xcc %swift_src_root/stdlib/public/Cxx/std \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}esc.h

// UNSUPPORTED: OS=windows-msvc

// Unknown escapability is the root cause of most unsafety, so it is reported in
// preference to whichever field the safety walk happened to stop on. For a
// conditionally-escapable type such as std::shared_ptr that matters: its own
// fields are implementation details, and the argument is what the user controls.
//
// The note on 'shared_ptr' itself is suppressed because it would land in the C++
// standard library headers, which the user cannot annotate. Only the note naming
// the type they control is emitted; it fires once per unsafe expression.

//--- Inputs/module.modulemap
module Esc {
    header "esc.h"
    requires cplusplus
}

//--- Inputs/esc.h
#include "swift/bridging"
#include <memory>

// Not copyable and polymorphic, so escapability cannot be derived from members.
// expected-note@+1 4 {{this type has unknown escapability: Swift cannot infer it from the type's members; annotate the type with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct Polymorphic {
  virtual ~Polymorphic() {}
  Polymorphic(const Polymorphic &) = delete;
  Polymorphic() {}
};

inline std::shared_ptr<Polymorphic> makePolymorphic() { return nullptr; }

// The chain is followed to its end, however deep: both links here are in the
// C++ standard library, so only the note on 'Polymorphic' survives.
inline std::shared_ptr<std::shared_ptr<Polymorphic>> makeNested() {
  return nullptr;
}

// When a member is what makes escapability underivable, the note names it.
// expected-note@+1 2 {{this type has unknown escapability: its member 'numbers' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct HasPointerMember {
  int *numbers;
};
inline HasPointerMember makeHasPointerMember() { return {}; }

// Annotating the argument settles it, and nothing here is unsafe.
struct SWIFT_ESCAPABLE Annotated {
  virtual ~Annotated() {}
  Annotated(const Annotated &) = delete;
  Annotated() {}
};

inline std::shared_ptr<Annotated> makeAnnotated() { return nullptr; }

struct SWIFT_NONESCAPABLE View {
  const int *p;
};

// Every note below lands on this one line, so they are stacked here. Buffer
// itself is only ever the first link in the chain.
// expected-note@+4 2 {{this type has unknown escapability: it depends on 'DtorWithView', whose escapability is unknown}}
// expected-note@+3 2 {{this type has unknown escapability: it depends on 'DtorWrapsHoldsView', whose escapability is unknown}}
// expected-note@+2 2 {{this type has unknown escapability: it depends on 'DtorWithBufferOfView', whose escapability is unknown}}
template <typename T>
struct Buffer {
  T *data;
  unsigned n;
} SWIFT_ESCAPABLE_IF(T);

// A record that provides its own destruction, so its non-escapable member
// cannot settle it. Reaching it through a container keeps it imported, and the
// chain ends on the record the user can annotate.
// expected-note@+1 2 {{this type has unknown escapability: its member 'v' is non-escapable, but it provides its own copy, move or destruction, so Swift cannot tell whether it is a view; annotate it with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct DtorWithView {
  View v;
  ~DtorWithView();
};

struct HoldsView {
  View v;
};

// The non-escapable type sits below the record that demoted it, so the note
// blames the record without naming a member of it: 'v' belongs to HoldsView,
// and HoldsView is plainly non-escapable rather than unknown.
// expected-note@+1 2 {{this type has unknown escapability: it holds a non-escapable type, but it provides its own copy, move or destruction, so Swift cannot tell whether it is a view; annotate it with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct DtorWrapsHoldsView {
  HoldsView h;

  ~DtorWrapsHoldsView();
};

// The non-escapable type arrives as a conditional template argument, which
// records no member to name, so the note still has to blame this record rather
// than the Buffer it came through.
// expected-note@+1 2 {{this type has unknown escapability: it holds a non-escapable type, but it provides its own copy, move or destruction, so Swift cannot tell whether it is a view; annotate it with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct DtorWithBufferOfView {
  Buffer<View> b;

  ~DtorWithBufferOfView();
};

using BufferOfDtorWithView = Buffer<DtorWithView>;
using BufferOfDtorWrapsHoldsView = Buffer<DtorWrapsHoldsView>;
using BufferOfDtorWithBufferOfView = Buffer<DtorWithBufferOfView>;
inline BufferOfDtorWrapsHoldsView makeBufferOfDtorWrapsHoldsView() {
  return {};
}
inline BufferOfDtorWithBufferOfView makeBufferOfDtorWithBufferOfView() {
  return {};
}
inline BufferOfDtorWithView makeBufferOfDtorWithView() { return {}; }

//--- test.swift
import Esc
import CxxStdlib

func unknownArgument() {
  let p = makePolymorphic() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makePolymorphic()' involves unsafe type}}
  _ = p // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'p' involves unsafe type}}
}

func nestedChain() {
  let p = makeNested() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeNested()' involves unsafe type}}
  _ = p // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'p' involves unsafe type}}
}

func pointerMember() {
  let h = makeHasPointerMember() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeHasPointerMember()' involves unsafe type}}
  _ = h // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'h' involves unsafe type}}
}

// An annotated argument needs no acknowledgement, which keeps this test honest
// about -strict-memory-safety being in effect above.
func annotatedArgument() {
  let p = makeAnnotated()
  _ = p
}

func nonEscapableMember() {
  let d = makeBufferOfDtorWithView() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeBufferOfDtorWithView()' involves unsafe type}}
  _ = d // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'd' involves unsafe type}}
}

// The blame has to reach the record whose own destruction is the problem, not
// whichever type happens to hold the non-escapable member.
func nonEscapableBelowTheDemotingRecord() {
  let d = makeBufferOfDtorWrapsHoldsView() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeBufferOfDtorWrapsHoldsView()' involves unsafe type}}
  _ = d // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'd' involves unsafe type}}
}

func nonEscapableViaConditionalArgument() {
  let d = makeBufferOfDtorWithBufferOfView() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeBufferOfDtorWithBufferOfView()' involves unsafe type}}
  _ = d // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to let 'd' involves unsafe type}}
}
