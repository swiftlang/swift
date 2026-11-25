// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}spec.h

// UNSUPPORTED: OS=windows-msvc

// Naming a class template specialization does not instantiate it, and until it
// is instantiated it carries none of its template's attributes. A specialization
// that only appears as a template argument -- an Owner<X> inside a
// Buffer<Owner<X>> that only points to its elements, say -- used to come out
// with unknown escapability, making everything around it unsafe. When the
// template's pattern is known, its annotations now apply.

//--- Inputs/module.modulemap
module Spec {
    header "spec.h"
    requires cplusplus
}

//--- Inputs/spec.h
#include "swift/bridging"

// Owns its storage through a raw pointer, escapable exactly when its element
// is. Holding only a T* does not instantiate T.
template <typename T>
struct Buffer {
  T *data;
  unsigned size;
} SWIFT_ESCAPABLE_IF(T);

template <typename T>
struct SWIFT_ESCAPABLE Owner {
  Owner(const Owner &);
  ~Owner();

private:
  T *ptr;
};

struct Opaque;

// The primary template has a partial specialization, so which one is the
// pattern of PartiallySpecialized<int> is only decided on instantiation.
template <typename T>
struct SWIFT_ESCAPABLE PartiallySpecialized {
  T *ptr;
};

template <typename T>
struct PartiallySpecialized<T *> {
  T **ptr;
};

// An explicit specialization has its own attributes, and declares none here.
template <typename T>
struct SWIFT_ESCAPABLE ExplicitlySpecialized {
  T *ptr;
};

// expected-note@+2 {{this type has unknown escapability: Swift cannot infer it from the type's members}}
template <>
struct ExplicitlySpecialized<int>;

struct NoDtor {
  int value;
};

// A member template of a class template is instantiated with the enclosing
// class, but without its swift_attrs.
template <typename U>
struct Outer {
  template <typename T>
  struct SWIFT_ESCAPABLE Inner {
    T *ptr;
  };
  U value;
};

// The annotation is on a redeclaration that comes after the specialization is
// first named.
template <typename T>
struct Late;

struct HoldsLate {
  Buffer<Late<Opaque>> values;
};

template <typename T>
struct SWIFT_ESCAPABLE Late;

// Copyability is read from the template as well.
template <typename T>
struct CopyableBuffer {
  T *data;
} SWIFT_ESCAPABLE_IF(T) SWIFT_COPYABLE_IF(T);

template <typename T>
struct SWIFT_ESCAPABLE SWIFT_NONCOPYABLE NonCopyable {
  T *ptr;
};

struct HoldsNonCopyable {
  CopyableBuffer<NonCopyable<Opaque>> values;
};

struct HoldsCopyable {
  CopyableBuffer<Owner<Opaque>> values;
};

// A foreign reference type is imported as a class, so it is always safe.
template <typename T>
struct SWIFT_IMMORTAL_REFERENCE Immortal {
  T *ptr;
};

struct HoldsImmortal {
  Buffer<Immortal<Opaque>> values;
};

// Owner<Opaque> is never instantiated: Opaque is incomplete, and Buffer only
// points to it.
struct HoldsOwner {
  Buffer<Owner<Opaque>> owners;
};

// The inner Buffer<NoDtor> is never instantiated, so its conditional
// annotation has to be found on the template too.
struct HoldsNestedBuffer {
  Buffer<Buffer<NoDtor>> buffers;
};

struct HoldsInner {
  Buffer<Outer<int>::Inner<Opaque>> values;
};

// expected-note@+1 {{this type has an unsafe field 'data'}}
struct HoldsPartiallySpecialized {
  Buffer<PartiallySpecialized<int>> values;
};

// expected-note@+1 {{this type has unknown escapability: it depends on 'ExplicitlySpecialized', whose escapability is unknown}}
struct HoldsExplicitlySpecialized {
  Buffer<ExplicitlySpecialized<int>> values;
};

inline HoldsOwner makeHoldsOwner() { return {}; }
inline HoldsNestedBuffer makeHoldsNestedBuffer() { return {}; }
inline HoldsInner makeHoldsInner() { return {}; }
inline HoldsLate makeHoldsLate() { return {}; }
inline HoldsImmortal makeHoldsImmortal() { return {}; }
inline HoldsNonCopyable makeHoldsNonCopyable() { return {}; }
inline HoldsCopyable makeHoldsCopyable() { return {}; }
inline HoldsPartiallySpecialized makeHoldsPartiallySpecialized() { return {}; }
inline HoldsExplicitlySpecialized makeHoldsExplicitlySpecialized() {
  return {};
}

//--- test.swift
import Spec

func patternIsKnown() {
  _ = makeHoldsOwner()
  _ = makeHoldsNestedBuffer()
  _ = makeHoldsInner()
  _ = makeHoldsLate()
  _ = makeHoldsImmortal()
}

func needsCopy<T>(_: T) {} // expected-note {{'where T: Copyable' is implicit here}}

func copyability() {
  needsCopy(makeHoldsCopyable())
  needsCopy(makeHoldsNonCopyable()) // expected-error {{global function 'needsCopy' requires that 'HoldsNonCopyable' conform to 'Copyable'}}
}

func patternIsNotKnown() {
  _ = makeHoldsPartiallySpecialized() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeHoldsPartiallySpecialized()' involves unsafe type}}
  _ = makeHoldsExplicitlySpecialized() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to global function 'makeHoldsExplicitlySpecialized()' involves unsafe type}}
}
