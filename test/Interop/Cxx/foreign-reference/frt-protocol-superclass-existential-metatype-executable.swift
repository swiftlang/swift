// Runtime behavior of `any P.Type` where `P`'s superclass bound is a foreign
// reference type.

// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

import FRTProtocolSuperclass
import StdlibUnittest

var Tests = TestSuite("FRTExistentialMetatype")

// MARK: shared FRT bound, @thin metatype

protocol SharedMeta: SharedBase {
  static func describe() -> CInt
}

extension SharedBase: SharedMeta {
  static func describe() -> CInt { 7 }
}

Tests.test("static dispatch through an existential metatype, shared FRT") {
  let m: any SharedMeta.Type = SharedBase.self
  expectEqual(7, m.describe())

  expectEqual(7, SharedBase.describe())

  // A metatype is not an object, so nothing here should touch the C++
  // reference count. This file never creates a SharedBase instance.
  expectEqual(0, SharedBase.numRefs())
  expectEqual(0, SharedBase.numDerefs())
}

// MARK: immortal FRT bound, also @thin

protocol ImmortalMeta: ImmortalBase {
  static func describe() -> CInt
}

extension ImmortalBase: ImmortalMeta {
  static func describe() -> CInt { 8 }
}

Tests.test("static dispatch through an existential metatype, immortal FRT") {
  let m: any ImmortalMeta.Type = ImmortalBase.self
  expectEqual(8, m.describe())
  expectEqual(8, ImmortalBase.describe())

  // If a metatype were mistaken for an object, a retain would land on the
  // canary or just past the end of the object.
  expectEqual(0xC0FFEE, ImmortalBase.shared().canary())
}

runAllTests()
