// RUN: %target-run-simple-swift-split-file(base.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)
// RUN: %target-run-simple-swift-split-file(not-base.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)
// RUN: %target-run-simple-swift-split-file(derived.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)
// RUN: %target-run-simple-swift-split-file(not-derived.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)

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
