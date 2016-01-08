// Casts.swift - Tests for conversion between types.
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// Contains tests for conversions between types which shouldn't trap.
///
// -----------------------------------------------------------------------------
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

let CastsTests = TestSuite("Casts")

// Test for SR-426: missing release for some types after failed conversion
class DeinitTester {
    private let onDeinit: () -> ()
    
    init(onDeinit: () -> ()) {
        self.onDeinit = onDeinit
    }
    deinit {
        onDeinit()
    }
}

func testFailedTupleCast(onDeinit: () -> ()) {
    // This function is to establish a scope for t to 
    // be deallocated at the end of.
    let t: Any = (1, DeinitTester(onDeinit: onDeinit))
    _ = t is Any.Type
}

CastsTests.test("No leak for failed tuple casts") {
    var deinitRan = false
    testFailedTupleCast {
        deinitRan = true
    }
    expectTrue(deinitRan)
}

runAllTests()