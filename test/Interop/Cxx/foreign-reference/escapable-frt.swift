// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}test.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -iapinotes-modules -Xcc %swift_src_root/stdlib/public/Cxx/std \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -strict-memory-safety

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
