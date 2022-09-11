// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// https://github.com/apple/swift/issues/58199
// Error when referencing #dsohandle in a Swift test on Windows
//
// This file tests that #dsohandle is fully usable from the built test. The
// precise value of #dsohandle is uninteresting.

import StdlibUnittest

var tests = TestSuite("#dsohandle usable")

tests.test("#dsohandle usable") {
    expectNotNil(#dsohandle as UnsafeRawPointer?)
}

runAllTests()
