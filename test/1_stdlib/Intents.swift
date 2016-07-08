// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=tvos

// FIXME: due to the lack of availability attributes in the SDK this test does
//        not fail on macOS Sierra, but does fail on El Capitan. Marking macOS
//        as unsupported for now.
// UNSUPPORTED: OS=macosx

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
