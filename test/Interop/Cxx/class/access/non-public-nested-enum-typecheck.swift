// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers -module-name main %t/base.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers -module-name main %t/derived.swift

// REQUIRES: swift_feature_ImportNonPublicCxxMembers

//--- Inputs/module.modulemap
module CxxModule {
    requires cplusplus
    header "cxx-header.h"
}

//--- Inputs/cxx-header.h
#pragma once

class __attribute__((__swift_attr__("private_fileid:main/base.swift")))
Base {
protected:
  enum class Enum { Foo, Bar };
public:
  Enum makeEnum(void) const { return Enum::Bar; }
};

class __attribute__((__swift_attr__("private_fileid:main/derived.swift")))
Derived : public Base {};

//--- base.swift
import CxxModule

extension Base {
  private static func inside(e: Enum) {
    let _: Enum = Enum.Bar
    let _: Base.Enum = Base.Enum.Bar
    let _: Enum = Base.Enum.Foo
    let _: Base.Enum = Enum.Foo

    switch e {
      case .Bar: return
      case .Foo: return
    }
  }
}

func switches() {
  let b = Base()
  let d = Derived()
  switch b.makeEnum() {
    default: return   // this is OK
  }
  switch d.makeEnum() {
    default: return   // this is OK
  }
  switch b.makeEnum() {
    case Base.Enum.Bar: return // expected-error {{'Enum' is inaccessible due to 'private' protection level}}
    case Base.Enum.Foo: return // expected-error {{'Enum' is inaccessible due to 'private' protection level}}
  }
  switch b.makeEnum() {
    case .Bar: return // OK as long as the user does not refer to Enum itself
    case .Foo: return // (NOTE: arguably this should not be allowed)
  }
  switch d.makeEnum() {
    case Derived.Enum.Bar: return // expected-error {{'Enum' is inaccessible due to 'private' protection level}}
    case Derived.Enum.Foo: return // expected-error {{'Enum' is inaccessible due to 'private' protection level}}
  }
  switch d.makeEnum() {
    case .Bar: return // OK as long as the user does not refer to Enum itself
    case .Foo: return // (NOTE: arguably this should not be allowed)
  }
}

func outside() {
  let b = Base()
  let d = Derived()
  let _ = b.makeEnum()  // This is OK as long as we don't name the type
  let _ = d.makeEnum()  // This is OK as long as we don't name the type

  let _: Derived.Enum = b.makeEnum() // expected-error {{'Enum' is inaccessible due to 'private' protection level}}
  let _: Derived.Enum = d.makeEnum() // expected-error {{'Enum' is inaccessible due to 'private' protection level}}

  // The types in the base and derived types should be considered the same type
  var x = b.makeEnum()
  x = d.makeEnum()
  var y = d.makeEnum()
  y = b.makeEnum()
}

//--- derived.swift
import CxxModule

extension Base {
  private static func inside(e: Enum) { // expected-error    {{'Enum' is inaccessible due to 'private' protection level}}
    let _: Enum = Enum.Bar              // expected-error    {{'Enum' is inaccessible due to 'private' protection level}}
                                        // expected-error@-1 {{'Enum' is inaccessible due to 'private' protection level}}
  }
}

extension Derived {
  private static func inside(e: Enum) {
    let b = Base()
    let _: Enum = Enum.Bar
    let _: Derived.Enum = Derived.Enum.Bar
    let _: Enum = b.makeEnum()

    // It would be nice to make these work but they do not
    let _: Base.Enum = Base.Enum.Bar // expected-error    {{'Enum' is inaccessible due to 'private' protection level}}
                                     // expected-error@-1 {{'Enum' is inaccessible due to 'private' protection level}}
    let _: Enum = Base.Enum.Foo      // expected-error    {{'Enum' is inaccessible due to 'private' protection level}}
    let _: Base.Enum = Enum.Foo      // expected-error    {{'Enum' is inaccessible due to 'private' protection level}}

    switch e {
      case .Bar: return
      case .Foo: return
    }
  }
}
