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
CastsTests.test("No leak for failed tuple casts") {
    let t: Any = (1, LifetimeTracked(0))
    expectFalse(t is Any.Type)
}

protocol P {}
class ErrClass : ErrorProtocol { }

CastsTests.test("No overrelease of existential boxes in failed casts") {
    // Test for crash from SR-392
    // We fail casts of an existential box repeatedly
    // to ensure it does not get over-released.
    func bar<T>(_ t: T) {
        for _ in 0..<10 {
            if case let a as P = t {
                _ = a
            }
        }
    }
    
    let err: ErrorProtocol = ErrClass()
    bar(err)
}

runAllTests()
