// Fields inherited via private inheritance should be readable and writable
// where Swift access control allows it.
//
// RUN: %target-run-simple-swift-split-file(blessed.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift-split-file(blessed.swift -I %t/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

//--- Inputs/module.modulemap
module CxxModule {
  requires cplusplus
  header "cxx-header.h"
}

//--- Inputs/cxx-header.h
#pragma once

class Base {
protected:
  int protField = 1;

public:
  int publField = 2;
};

class __attribute__((__swift_attr__("private_fileid:main/blessed.swift")))
Priv : private Base {};

class __attribute__((__swift_attr__("private_fileid:main/blessed.swift")))
Prot : protected Base {};

//--- blessed.swift
import StdlibUnittest
import CxxModule

var Suite = TestSuite("Fields inherited via non-public inheritance")

extension Priv {
  mutating func bump() -> CInt {
    protField += 10
    publField += 20
    return protField + publField
  }
}

extension Prot {
  mutating func bump() -> CInt {
    protField += 10
    publField += 20
    return protField + publField
  }
}

Suite.test("private inheritance") {
  var p = Priv()
  expectEqual(p.bump(), 33)
}

Suite.test("protected inheritance") {
  var p = Prot()
  expectEqual(p.bump(), 33)
}

runAllTests()
