// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
  init() { NoisyErrorLifeCount += 1 }
  deinit { NoisyErrorDeathCount += 1 }

  let _domain = "NoisyError"
  let _code = 123

  let otherProperty = "otherProperty"
  let otherClassProperty = "otherClassProperty"
}

ErrorTypeTests.test("erasure") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let e: ErrorType = NoisyError()

    expectEqual(e._domain, "NoisyError")
    expectEqual(e._code, 123)
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
    expectEqual((op as! ErrorType)._domain, "NoisyError")
    expectEqual((op as! ErrorType)._code, 123)

    let ocp: OtherClassProtocol = ne
    expectEqual((ocp as! ErrorType)._domain, "NoisyError")
    expectEqual((ocp as! ErrorType)._code, 123)

    // Do the same with rvalues, so we exercise the
    // take-on-success/destroy-on-failure paths.

    expectEqual(((NoisyError() as ErrorType) as! NoisyError)._domain, "NoisyError")
    expectEqual(((NoisyError() as ErrorType) as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual(((NoisyError() as ErrorType) as! OtherProtocol).otherProperty, "otherProperty")

    expectEqual(((NoisyError() as OtherProtocol) as! ErrorType)._domain, "NoisyError")
    expectEqual(((NoisyError() as OtherProtocol) as! ErrorType)._code, 123)

    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorType)._domain, "NoisyError")
    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorType)._code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

struct DefaultStruct : ErrorType { }
class DefaultClass : ErrorType { }

ErrorTypeTests.test("default domain and code") {
  expectEqual(DefaultStruct()._domain, "main.DefaultStruct")
  expectEqual(DefaultStruct()._code, 1)
  expectEqual(DefaultClass()._domain, "main.DefaultClass")
  expectEqual(DefaultClass()._code, 1)
}

enum SillyError: ErrorType { case JazzHands }

ErrorTypeTests.test("try!")
  .skip(.Custom({ _isFastAssertConfiguration() },
                reason: "trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration()
                        ? "'try!' expression unexpectedly raised an error: "
                          + "main.SillyError.JazzHands"
                        : "")
  .code {
    expectCrashLater()
    let _: () = try! { throw SillyError.JazzHands }()
  }

ErrorTypeTests.test("try?") {
  var value = try? { () throws -> Int in return 1 }()
  expectType(Optional<Int>.self, &value)
  expectEqual(Optional(1), value)

  expectEmpty(try? { () throws -> Int in throw SillyError.JazzHands }())
}

enum LifetimeError : ErrorType {
  case MistakeOfALifetime(LifetimeTracked, yearsIncarcerated: Int)
}

ErrorTypeTests.test("existential in lvalue") {
  expectEqual(0, LifetimeTracked.instances)
  do {
    var e: ErrorType? = nil
    do {
      throw LifetimeError.MistakeOfALifetime(LifetimeTracked(0),
                                             yearsIncarcerated: 25)
    } catch {
      e = error
    }
    expectEqual(1, LifetimeTracked.instances)
    expectEqual(0, e?._code)
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()

