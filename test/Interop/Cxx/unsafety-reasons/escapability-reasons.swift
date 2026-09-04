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
// expected-note@+1 4 {{type 'Polymorphic' has unknown escapability because Swift cannot infer it from the type's members; annotate the type with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
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
// expected-note@+1 2 {{type 'HasPointerMember' has unknown escapability because its member 'numbers' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
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
