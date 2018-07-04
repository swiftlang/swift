// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift %t/main.swift %S/Inputs/CodableMultifileOther.swift -module-name main -o %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

// FIXME: This test could run on Linux too, if we could either use
// corelibs-foundation, or implement a mock Encoder for testing.

// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var CodableMultifileTestSuite = TestSuite("CodableMultifile")

// The first test doesn't synthesize encode(to:) at all.
CodableMultifileTestSuite.test("1") {
  let derived = DerivedFirst()

  // Make sure the vtable offset matches between this translation unit and
  // the other file, which requires us to force the encode(to:) member when
  // type checking this translation unit.
  expectEqual(false, derived.derivedMember)
}

// The second test synthesizes init(from:) before encode(to:).

// We define a wrapper so that the virtual method BaseSecond.encode(to:) method
// is called from outside its module. If we use the conformance of BaseSecond
// to Codable, we don't expose the bug because the virtual method call is
// encapsulated in the conformance, which is emitted in the same translation unit
// as BaseSecond.
struct WrapperSecond : Encodable {
  let base: BaseSecond

  func encode(to encoder: Encoder) throws {
    try base.encode(to: encoder)
  }
}

CodableMultifileTestSuite.test("2") {
  // Make sure we synthesize the init(from:) member before encode(to:) in
  // this translation unit.
  _ = BaseSecond.init(from:)

  let base = WrapperSecond(base: BaseSecond())
  let encoder = JSONEncoder()

  expectEqual(
    "{\"baseMember\":2}",
    String(data: try! encoder.encode(base), encoding: .utf8)!)
}

// The third test synthesizes encode(to:) before init(from:).

// See above.
struct WrapperThird : Encodable {
  let base: BaseThird

  func encode(to encoder: Encoder) throws {
    try base.encode(to: encoder)
  }
}

CodableMultifileTestSuite.test("3") {
  // Make sure we synthesize the encode(to:) member before init(from:) in
  // this translation unit.
  _ = BaseThird.encode(to:)

  let base = WrapperThird(base: BaseThird())
  let encoder = JSONEncoder()

  expectEqual(
    "{\"baseMember\":3}",
    String(data: try! encoder.encode(base), encoding: .utf8)!)
}

runAllTests()
