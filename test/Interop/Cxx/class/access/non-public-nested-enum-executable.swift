// RUN: split-file %s %t

// RUN: %target-build-swift -module-name main %t/base.swift -I %t/Inputs -o %t/base -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// RUN: %target-codesign %t/base
// RUN: %target-run %t/base

// RUN: %target-build-swift -module-name main %t/not-base.swift -I %t/Inputs -o %t/not-base -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// RUN: %target-codesign %t/not-base
// RUN: %target-run %t/not-base

// RUN: %target-build-swift -module-name main %t/derived.swift -I %t/Inputs -o %t/derived -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// RUN: %target-codesign %t/derived
// RUN: %target-run %t/derived

// RUN: %target-build-swift -module-name main %t/not-derived.swift -I %t/Inputs -o %t/not-derived -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// RUN: %target-codesign %t/not-derived
// RUN: %target-run %t/not-derived

// REQUIRES: executable_test
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
import StdlibUnittest
import CxxModule

var Suite = TestSuite("BlessedForBase")
extension Base {
  static func makeEnums() {
    let e1: Enum = Enum.Bar
    let e2: Enum = Enum.Foo
    expectEqual(e1, Enum.Bar)
    expectNotEqual(e1, e2)
    switch e2 {
    case .Bar:
      expectTrue(false, "foo is bar")
    case .Foo:
      expectTrue(true, "foo is foo")
    }
  }
}
Suite.test("Use private nested enum in base class") { Base.makeEnums() }
runAllTests()

//--- not-base.swift
import StdlibUnittest
import CxxModule

var Suite = TestSuite("NotBlessedForBase")
Suite.test("Use private nested enum in base class") {
  let b = Base()
  let e1 = b.makeEnum()
  let e2 = b.makeEnum()
  expectEqual(e1, e2)
}
runAllTests()


//--- derived.swift
import StdlibUnittest
import CxxModule

var Suite = TestSuite("BlessedForDerived")
extension Derived {
  static func makeEnums() {
    let e1: Enum = Enum.Bar
    let e2: Enum = Enum.Foo
    expectEqual(e1, Enum.Bar)
    expectNotEqual(e1, e2)
    switch e2 {
    case .Bar:
      expectTrue(false, "foo is bar")
    case .Foo:
      expectTrue(true, "foo is foo")
    }
  }
}
Suite.test("Use private nested enum inherited from base class") { Derived.makeEnums() }
runAllTests()

//--- not-derived.swift
import StdlibUnittest
import CxxModule

var Suite = TestSuite("NotBlessedForDerive")
Suite.test("Use private nested enum inherited from base class") {
  let d = Derived()
  let e1 = d.makeEnum()
  let e2 = d.makeEnum()
  expectEqual(e1, e2)
}
runAllTests()
