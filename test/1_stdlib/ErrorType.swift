// RUN: %target-run-simple-swift

import StdlibUnittest
import Foundation

var ErrorTypeTests = TestSuite("ErrorType")

var NoisyErrorDeathCount = 0

class NoisyError: _ErrorType {
  deinit { ++NoisyErrorDeathCount }

  let domain = "NoisyError"
  let code = 123
}

ErrorTypeTests.test("erasure") {
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let e: _ErrorType = NoisyError()

    expectEqual(e.domain, "NoisyError")
    expectEqual(e.code, 123)
  }
  expectEqual(NoisyErrorDeathCount, 1)
}

ErrorTypeTests.test("reflection") {
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ne = NoisyError()
    let e: _ErrorType = ne

    var neDump = "", eDump = ""
    dump(ne, &neDump)
    dump(e, &eDump)

    expectEqual(eDump, neDump)
  }
  expectEqual(NoisyErrorDeathCount, 1)
}

runAllTests()

