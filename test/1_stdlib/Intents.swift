// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=tvos

// XFAIL: OS=macosx

import Intents
import StdlibUnittest

let IntentsTestSuite = TestSuite("Intents")

IntentsTestSuite.test("ErrorDomain") {
  expectEqual("IntentsErrorDomain", INIntentErrorDomain)
}

IntentsTestSuite.test("extension") {
  expectEqual("IntentsErrorDomain", INIntentErrorCode._nsErrorDomain)
}

runAllTests()
