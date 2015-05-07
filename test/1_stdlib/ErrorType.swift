// RUN: %target-run-simple-swift

import StdlibUnittest

var ErrorTypeTests = TestSuite("ErrorType")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0

protocol OtherProtocol {
  var otherProperty: String { get }
}

protocol OtherClassProtocol : class {
  var otherClassProperty: String { get }
}

class NoisyError : ErrorType, OtherProtocol, OtherClassProtocol {
  init() { ++NoisyErrorLifeCount }
  deinit { ++NoisyErrorDeathCount }

  let domain = "NoisyError"
  let code = 123

  let otherProperty = "otherProperty"
  let otherClassProperty = "otherClassProperty"
}

ErrorTypeTests.test("erasure") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let e: ErrorType = NoisyError()

    expectEqual(e.domain, "NoisyError")
    expectEqual(e.code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorTypeTests.test("reflection") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let ne = NoisyError()
    let e: ErrorType = ne

    var neDump = "", eDump = ""
    dump(ne, &neDump)
    dump(e, &eDump)

    expectEqual(eDump, neDump)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorTypeTests.test("dynamic casts") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let ne = NoisyError()
    let e: ErrorType = ne

    expectTrue(e as! NoisyError === ne)
    expectEqual((e as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual((e as! OtherProtocol).otherProperty, "otherProperty")

    let op: OtherProtocol = ne
    expectEqual((op as! ErrorType).domain, "NoisyError")
    expectEqual((op as! ErrorType).code, 123)

    let ocp: OtherClassProtocol = ne
    expectEqual((ocp as! ErrorType).domain, "NoisyError")
    expectEqual((ocp as! ErrorType).code, 123)

    // Do the same with rvalues, so we exercise the
    // take-on-success/destroy-on-failure paths.

    expectEqual(((NoisyError() as ErrorType) as! NoisyError).domain, "NoisyError")
    expectEqual(((NoisyError() as ErrorType) as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual(((NoisyError() as ErrorType) as! OtherProtocol).otherProperty, "otherProperty")

    expectEqual(((NoisyError() as OtherProtocol) as! ErrorType).domain, "NoisyError")
    expectEqual(((NoisyError() as OtherProtocol) as! ErrorType).code, 123)

    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorType).domain, "NoisyError")
    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorType).code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

runAllTests()

