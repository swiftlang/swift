// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -iapinotes-modules -Xcc %swift_src_root/stdlib/public/Cxx/std \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -strict-memory-safety \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}escapable.h

// UNSUPPORTED: OS=windows-msvc

// Foreign reference types are always escapable, so member-based inference must
// not report unknown escapability for them, which would make containers such as
// std::shared_ptr unsafe under strict memory safety.

//--- Inputs/module.modulemap
module Test {
    header "escapable.h"
    requires cplusplus
}

//--- Inputs/escapable.h
#include "swift/bridging"
#include <memory>

// A typical foreign reference base: polymorphic and non-copyable, so
// escapability cannot be derived from its members.
struct SWIFT_IMMORTAL_REFERENCE Base {
  virtual ~Base() {}
  Base(const Base &) = delete;
  Base &operator=(const Base &) = delete;

protected:
  Base() {}
};

// Reference-ness is inherited from Base, so Derived is escapable as well.
struct Derived : Base {
  int value;
};

// A foreign reference type annotated directly.
struct SWIFT_IMMORTAL_REFERENCE Immortal {
  virtual ~Immortal() {}
  int value;
};

// Same shape, but not a reference type: escapability stays unknown, so this one
// is still unsafe. Keeps the test honest about -strict-memory-safety being on.
// expected-note@+1 2 {{this type has unknown escapability: Swift cannot infer it from the type's members; annotate the type with SWIFT_SELF_CONTAINED or SWIFT_VIEW}}
struct Polymorphic {
  virtual ~Polymorphic() {}
  Polymorphic(const Polymorphic &) = delete;
  Polymorphic &operator=(const Polymorphic &) = delete;
  Polymorphic() {}
};

inline std::shared_ptr<Derived> makeDerived() { return nullptr; }
inline std::shared_ptr<Immortal> makeImmortal() { return nullptr; }
inline std::unique_ptr<Derived> makeUniqueDerived() { return nullptr; }
inline std::shared_ptr<Polymorphic> makePolymorphic() { return nullptr; }

// A templated foreign reference type behaves as if it was annotated with
// SWIFT_SELF_CONTAINED_IF over all of its template parameters, so its template
// arguments decide its safety as they would for a value type, even when they are
// not stored by value. A non-escapable argument would make it a view, which a
// Swift class cannot be. An argument of unknown escapability makes it unsafe if
// the argument is unsafe.
struct SWIFT_NONESCAPABLE View {
  int *p;
};
struct SWIFT_ESCAPABLE Owner {
  int x;
};
// expected-note@+1 2 {{this type has unknown escapability: its member 'p' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct HasPointer {
  int *p;
};

// expected-note@+3 {{this type is a foreign reference type with non-'Escapable' template argument 'View', but Swift classes are always escapable}}
// expected-note@+2 {{this type has an unsafe template argument 'int *'}}
// expected-note@+1 {{this type has an unsafe template argument 'HasPointer'}}
template <class T> struct SWIFT_IMMORTAL_REFERENCE Holder {
  T *p;
};
using HolderView = Holder<View>;
using HolderIntPointer = Holder<int *>;
using HolderHasPointer = Holder<HasPointer>;
using HolderInt = Holder<int>;
using HolderOwner = Holder<Owner>;
using HolderImmortal = Holder<Immortal>;
using HolderImmortalPointer = Holder<Immortal *>;
using HolderPolymorphic = Holder<Polymorphic>;

// The unsafety is inherited by derived classes, and the note names the base.
// expected-note@+1 {{base 'Holder<View>' is a foreign reference type with non-'Escapable' template argument 'View', but Swift classes are always escapable}}
struct DerivedFromHolderView : Holder<View> {};
// expected-note@+1 {{base 'Holder<int *>' has an unsafe template argument 'int *'}}
struct DerivedFromHolderIntPointer : Holder<int *> {};
// expected-note@+1 {{base 'Holder<HasPointer>' has an unsafe template argument 'HasPointer'}}
struct DerivedFromHolderHasPointer : Holder<HasPointer> {};

// Template arguments that refer back to the class do not loop.
struct CRTP : Holder<CRTP> {};
struct Node {
  Holder<Node> *next;
};
using HolderNode = Holder<Node>;

// Parameter packs are checked too.
// expected-note@+1 {{this type is a foreign reference type with non-'Escapable' template argument 'View', but Swift classes are always escapable}}
template <class... Ts> struct SWIFT_IMMORTAL_REFERENCE Variadic {
  int x;
};
using VariadicView = Variadic<int, View>;
using VariadicOwner = Variadic<int, Owner>;

// SWIFT_SAFE overrides the inference, for derived classes too.
template <class T> struct SWIFT_IMMORTAL_REFERENCE SWIFT_SAFE Vouched {
  T *p;
};
using VouchedView = Vouched<View>;
struct DerivedFromVouchedView : Vouched<View> {};

//--- test.swift
import Test
import CxxStdlib

// A smart pointer to a foreign reference type is escapable, so using it does
// not require 'unsafe'.
public func useSharedDerived() {
  let ptr = makeDerived()
  _ = ptr
}

public func useSharedImmortal() {
  let ptr = makeImmortal()
  _ = ptr
}

public func useUniqueDerived() {
  let ptr = makeUniqueDerived()
  _ = ptr
}

// The foreign reference types themselves are imported as classes.
public func useDerived(_ x: Derived) -> Int32 { x.value }
public func useImmortal(_ x: Immortal) -> Int32 { x.value }

// A non-reference type of the same shape is still unsafe, which confirms
// -strict-memory-safety is in effect above.
public func usePolymorphic() {
  let ptr = makePolymorphic() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{involves unsafe type}}
  _ = ptr // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{involves unsafe type}}
}

public func useUnsafeHolders(_ a: HolderView, _ b: HolderIntPointer,
                             _ c: HolderHasPointer,
                             _ d: DerivedFromHolderView,
                             _ e: DerivedFromHolderIntPointer,
                             _ f: DerivedFromHolderHasPointer,
                             _ g: VariadicView) {
  _ = a // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'a' involves unsafe type 'HolderView'}}
  _ = b // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'b' involves unsafe type 'HolderIntPointer'}}
  _ = c // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'c' involves unsafe type 'HolderHasPointer'}}
  _ = d // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'd' involves unsafe type 'DerivedFromHolderView'}}
  _ = e // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'e' involves unsafe type 'DerivedFromHolderIntPointer'}}
  _ = f // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'f' involves unsafe type 'DerivedFromHolderHasPointer'}}
  _ = g // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-1 {{reference to parameter 'g' involves unsafe type 'VariadicView'}}
}

// Escapable arguments, foreign reference types, pointers to them, and arguments
// of unknown escapability that are not unsafe leave the class safe.
public func useSafeHolders(_ a: HolderInt, _ b: HolderOwner,
                           _ c: HolderImmortal, _ d: HolderImmortalPointer,
                           _ e: HolderPolymorphic, _ f: VariadicOwner,
                           _ g: VouchedView, _ h: DerivedFromVouchedView,
                           _ i: CRTP, _ j: HolderNode) {
  _ = (a, b, c, d, e, f, g, h, i, j)
}
