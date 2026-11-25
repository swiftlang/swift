// Members inherited from a C++ base class keep the attributes and implicitly
// unwrapped optionals that the base class's members are imported with.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default %t%{fs-sep}main.swift
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -strict-memory-safety %t%{fs-sep}strict.swift

//--- Inputs/module.modulemap
module InheritedAttributes {
  header "inherited-attributes.h"
  requires cplusplus
}

//--- Inputs/inherited-attributes.h
#pragma once

struct Base {
  int unavailableField __attribute__((unavailable)) = 0;
  int unsafeField __attribute__((swift_attr("unsafe"))) = 0;

  int *iuoField = nullptr;
  int *iuoMethod() const { return nullptr; }
  int *operator[](int) const { return nullptr; }
};

struct Derived : Base {};
struct DerivedFromDerived : Derived {};

//--- main.swift
import InheritedAttributes

func unavailable(b: Base, d: Derived, dd: DerivedFromDerived) {
  _ = b.unavailableField // expected-error {{'unavailableField' is unavailable}}
  _ = d.unavailableField // expected-error {{'unavailableField' is unavailable}}
  _ = dd.unavailableField // expected-error {{'unavailableField' is unavailable}}
}

func implicitlyUnwrapped(b: Base, d: Derived, dd: DerivedFromDerived) {
  let _: UnsafeMutablePointer<CInt> = b.iuoField
  let _: UnsafeMutablePointer<CInt> = d.iuoField
  let _: UnsafeMutablePointer<CInt> = dd.iuoField

  let _: UnsafeMutablePointer<CInt> = b.iuoMethod()
  let _: UnsafeMutablePointer<CInt> = d.iuoMethod()
  let _: UnsafeMutablePointer<CInt> = dd.iuoMethod()
}

func implicitlyUnwrappedSubscript(b: inout Base, d: inout Derived,
                                  dd: inout DerivedFromDerived) {
  let _: UnsafeMutablePointer<CInt> = b[0]
  let _: UnsafeMutablePointer<CInt> = d[0]
  let _: UnsafeMutablePointer<CInt> = dd[0]
}

//--- strict.swift
import InheritedAttributes

func unsafeField(b: Base, d: Derived, dd: DerivedFromDerived) {
  _ = b.unsafeField // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = d.unsafeField // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = dd.unsafeField // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = unsafe d.unsafeField
}
