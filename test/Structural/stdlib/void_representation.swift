// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Test dummy void-based conformance to `Structural` protocol.

import Swift
import StdlibUnittest
import _Structural

struct Dummy: Structural, Equatable {
  typealias StructuralRepresentation = Void

  init() {}

  init(structuralRepresentation: StructuralRepresentation) {}

  var structuralRepresentation: StructuralRepresentation {
    get { () }
    set(newValue) { }
  }
}

let VoidRepresentationTests = TestSuite("VoidRepresentation")

VoidRepresentationTests.test("init(structuralRepresentation:)") {
  let dummy1 = Dummy()
  let dummy2 = Dummy(structuralRepresentation: ())
  expectEqual(dummy1, dummy2)
}

VoidRepresentationTests.test("var structuralRepresentation") {
  let dummy = Dummy()
  let repr = dummy.structuralRepresentation
  expectTrue(repr == ())
}

runAllTests()
