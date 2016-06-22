// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Intents is only public on OS X and iOS
// UNSUPPORTED: OS=watchos, OS=tvos

// FIXME: un-XFAIL the test once the build bots have been updated to the
//        more recent SDK versions.
// XFAIL: *

import Intents
import StdlibUnittest

let IntentsTestSuite = TestSuite("Intents")

IntentsTestSuite.test("ErrorDomain") {
  expectEqual("INIntentErrorDomain", INIntentErrorDomain)
}

IntentsTestSuite.test("extension") {
  expectEqual("INIntentErrorDomain", INIntentErrorCode._nsErrorDomain)
}

runAllTests()
