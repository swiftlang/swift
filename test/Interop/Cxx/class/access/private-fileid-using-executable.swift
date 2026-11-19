// RUN: %target-run-simple-swift-split-file(blessed.swift -I %S/Inputs -Xfrontend -cxx-interoperability-mode=default)
// REQUIRES: executable_test

//--- blessed.swift

import StdlibUnittest
import PrivateFileIDUsing

var Suite = TestSuite("Data members redeclared with using")

extension PublUser {
  mutating func bumpNoUsing() -> CInt {
    protFieldNoUsing += 1
    return protFieldNoUsing
  }
}

extension PrivUser {
  mutating func bumpNoUsing() -> CInt {
    protFieldNoUsing += 1
    return protFieldNoUsing
  }

  mutating func bumpProtUsing() -> CInt {
    protFieldProtUsing += 1
    return protFieldProtUsing
  }
}

extension PublPrivUser {
  mutating func bumpProtUsing() -> CInt {
    protFieldProtUsing += 1
    return protFieldProtUsing
  }
}

Suite.test("PublUser") {
  var u = PublUser()
  expectEqual(u.protField, 1)
  u.protField = 42
  expectEqual(u.protField, 42)
  expectEqual(u.bumpNoUsing(), 3)
}

Suite.test("PrivUser") {
  var u = PrivUser()
  expectEqual(u.protField, 1)
  expectEqual(u.publField, 3)
  u.protField = 42
  u.publField = 43
  expectEqual(u.protField, 42)
  expectEqual(u.publField, 43)
  expectEqual(u.bumpNoUsing(), 3)
  expectEqual(u.bumpProtUsing(), 5)
}

Suite.test("PublPrivUser") {
  var u = PublPrivUser()
  expectEqual(u.protField, 1)
  expectEqual(u.publField, 3)
  u.protField = 42
  u.publField = 43
  expectEqual(u.protField, 42)
  expectEqual(u.publField, 43)
  expectEqual(u.bumpProtUsing(), 5)
}

runAllTests()
