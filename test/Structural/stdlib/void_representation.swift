// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: structural

// Test dummy void-based conformance to `Structural` protocol.

import Swift
import StdlibUnittest

struct Dummy: Structural, Equatable {
    typealias StructuralRepresentation = Void

    init() {
    }

    init(structuralRepresentation: StructuralRepresentation) {
    }

    var structuralRepresentation: StructuralRepresentation {
        get {
            return ()
        }
        set(newValue) {
        }
    }
}

let VoidRepresentationTests = TestSuite("VoidRepresentation") 

VoidRepresentationTests.test("init(structuralRepresentation:)") {
    let dummy1 = Dummy()
    let dummy2 = Dummy(structuralRepresentation: ())
    expectEqual(dummy1, dummy2)
}

VoidRepresentationTests.test("structuralRepresentation") {
    let dummy = Dummy()
    let repr: Void = dummy.structuralRepresentation
    expectTrue(repr == ())
}

runAllTests()
