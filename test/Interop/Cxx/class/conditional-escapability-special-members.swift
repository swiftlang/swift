// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -iapinotes-modules -Xcc %swift_src_root/stdlib/public/Cxx/std \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}cond.h

// UNSUPPORTED: OS=windows-msvc

// A SWIFT_ESCAPABLE_IF container used to be unsafe whenever its element provided
// any of its own special member functions -- a destructor was enough. The element
// was safe, but its escapability came out unknown, which cost the container its
// "self-contained, therefore safe" status and exposed the raw pointer the
// annotation exists to vouch for.
//
// Providing your own copy, move or destruction blocks only the non-escapable
// conclusion; nonescapable-errors.swift covers that side.

//--- Inputs/module.modulemap
module Cond {
    header "cond.h"
    requires cplusplus
}

//--- Inputs/cond.h
#include "swift/bridging"
#include <optional>
#include <utility>
#include <vector>

// Stands in for WTF::Vector: owns its storage through a raw pointer, escapable
// exactly when its element is.
// expected-note@+2 {{this type has unknown escapability: it depends on 'HasPointer', whose escapability is unknown}}
template <typename T>
struct Buffer {
  T *data;
  unsigned size;
} SWIFT_ESCAPABLE_IF(T);

struct NoDtor {
  int value;
};

// An ordinary owning value type: a user-declared destructor and nothing else.
struct WithDtor {
  int value;
  ~WithDtor() {}
};

// Declares no special member function of its own, only a member that does.
struct WrapsWithDtor {
  WithDtor inner;
};

// How a type with expensive-to-instantiate members usually spells it. Still
// user-provided as far as Clang is concerned.
struct DefaultedOutOfLineDtor {
  int value;
  ~DefaultedOutOfLineDtor();
};
inline DefaultedOutOfLineDtor::~DefaultedOutOfLineDtor() = default;

struct WithMoveOperations {
  int value;
  WithMoveOperations(WithMoveOperations &&other);
  WithMoveOperations &operator=(WithMoveOperations &&other);
};

// Asserts its own escapability, as WTF::String does.
struct SWIFT_ESCAPABLE Str {
  Str();
  Str(const Str &);
  Str &operator=(const Str &);
  ~Str();

private:
  void *impl;
};

// Escapability really is unknown: the pointer member forces it.
// expected-note@+1 {{this type has unknown escapability: its member 'p' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct HasPointer {
  int *p;
  ~HasPointer() {}
};

using BufferOfNoDtor = Buffer<NoDtor>;
using BufferOfWithDtor = Buffer<WithDtor>;
using BufferOfWrapsWithDtor = Buffer<WrapsWithDtor>;
using BufferOfDefaultedOutOfLineDtor = Buffer<DefaultedOutOfLineDtor>;
using BufferOfWithMoveOperations = Buffer<WithMoveOperations>;
using BufferOfHasPointer = Buffer<HasPointer>;
// The parameters injected for std types take the same path.
using VectorOfWithDtor = std::vector<WithDtor>;
// The shape this came from: WebKit's RunJavaScriptParameters::arguments.
using OptionalBufferOfPairs = std::optional<Buffer<std::pair<Str, WithDtor>>>;

inline BufferOfNoDtor makeBufferOfNoDtor() { return {}; }
inline BufferOfWithDtor makeBufferOfWithDtor() { return {}; }
inline BufferOfWrapsWithDtor makeBufferOfWrapsWithDtor() { return {}; }
inline BufferOfDefaultedOutOfLineDtor makeBufferOfDefaultedOutOfLineDtor() {
  return {};
}
inline BufferOfWithMoveOperations makeBufferOfWithMoveOperations() {
  return {};
}
inline BufferOfHasPointer makeBufferOfHasPointer() { return {}; }
inline VectorOfWithDtor makeVectorOfWithDtor() { return {}; }
inline OptionalBufferOfPairs makeOptionalBufferOfPairs() { return {}; }
inline WithDtor makeWithDtor() { return {}; }

//--- test.swift
import Cond
import CxxStdlib

// None of these is unsafe.
func owningElements() {
  _ = makeWithDtor()
  _ = makeBufferOfNoDtor()
  _ = makeBufferOfWithDtor()
  _ = makeBufferOfWrapsWithDtor()
  _ = makeBufferOfDefaultedOutOfLineDtor()
  _ = makeBufferOfWithMoveOperations()
  _ = makeVectorOfWithDtor()
  _ = makeOptionalBufferOfPairs()
}

// The element holds a pointer, so its escapability is unknown for a reason that
// has nothing to do with its destructor.
func unknownElement() {
  _ = makeBufferOfHasPointer() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeBufferOfHasPointer()' involves unsafe type}}
}
