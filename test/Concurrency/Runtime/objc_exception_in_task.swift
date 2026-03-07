// RUN: %empty-directory(%t)
// RUN: %target-build-swift -import-objc-header %S/Inputs/ObjCExceptionHelpers.h %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: objc_interop

// UNSUPPORTED: back_deployment_runtime

import Foundation
import StdlibUnittest

var tests = TestSuite("ObjCExceptionInTask")

tests.test("exception terminates in swift_job_runImpl") {
  expectCrashLater()

  Task { @MainActor in
    ThrowObjCException()
  }

  _ = CatchingException {
    RunLoop.current.run()
  }

  expectUnreachable("Exception should not propagate through swift_job_runImpl")
}

runAllTests()
