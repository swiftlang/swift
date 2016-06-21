// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var ErrorProtocolTests = TestSuite("ErrorProtocol")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0

protocol OtherProtocol {
  var otherProperty: String { get }
}

protocol OtherClassProtocol : class {
  var otherClassProperty: String { get }
}

class NoisyError : ErrorProtocol, OtherProtocol, OtherClassProtocol {
  init() { NoisyErrorLifeCount += 1 }
  deinit { NoisyErrorDeathCount += 1 }

  let _domain = "NoisyError"
  let _code = 123

  let otherProperty = "otherProperty"
  let otherClassProperty = "otherClassProperty"
}

ErrorProtocolTests.test("erasure") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let e: ErrorProtocol = NoisyError()

    expectEqual(e._domain, "NoisyError")
    expectEqual(e._code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorProtocolTests.test("reflection") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let ne = NoisyError()
    let e: ErrorProtocol = ne

    var neDump = "", eDump = ""
    dump(ne, to: &neDump)
    dump(e, to: &eDump)

    expectEqual(eDump, neDump)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorProtocolTests.test("dynamic casts") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  do {
    let ne = NoisyError()
    let e: ErrorProtocol = ne

    expectTrue(e as! NoisyError === ne)
    expectEqual((e as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual((e as! OtherProtocol).otherProperty, "otherProperty")

    let op: OtherProtocol = ne
    expectEqual((op as! ErrorProtocol)._domain, "NoisyError")
    expectEqual((op as! ErrorProtocol)._code, 123)

    let ocp: OtherClassProtocol = ne
    expectEqual((ocp as! ErrorProtocol)._domain, "NoisyError")
    expectEqual((ocp as! ErrorProtocol)._code, 123)

    // Do the same with rvalues, so we exercise the
    // take-on-success/destroy-on-failure paths.

    expectEqual(((NoisyError() as ErrorProtocol) as! NoisyError)._domain, "NoisyError")
    expectEqual(((NoisyError() as ErrorProtocol) as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual(((NoisyError() as ErrorProtocol) as! OtherProtocol).otherProperty, "otherProperty")

    expectEqual(((NoisyError() as OtherProtocol) as! ErrorProtocol)._domain, "NoisyError")
    expectEqual(((NoisyError() as OtherProtocol) as! ErrorProtocol)._code, 123)

    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorProtocol)._domain, "NoisyError")
    expectEqual(((NoisyError() as OtherClassProtocol) as! ErrorProtocol)._code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

struct DefaultStruct : ErrorProtocol { }
class DefaultClass : ErrorProtocol { }

ErrorProtocolTests.test("default domain and code") {
  expectEqual(DefaultStruct()._domain, "main.DefaultStruct")
  expectEqual(DefaultStruct()._code, 1)
  expectEqual(DefaultClass()._domain, "main.DefaultClass")
  expectEqual(DefaultClass()._code, 1)
}

enum SillyError: ErrorProtocol { case JazzHands }

ErrorProtocolTests.test("try!")
  .skip(.custom({ _isFastAssertConfiguration() },
                reason: "trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration()
                        ? "'try!' expression unexpectedly raised an error: "
                          + "main.SillyError.JazzHands"
                        : "")
  .code {
    expectCrashLater()
    let _: () = try! { throw SillyError.JazzHands }()
  }

ErrorProtocolTests.test("try?") {
  var value = try? { () throws -> Int in return 1 }()
  expectType(Optional<Int>.self, &value)
  expectEqual(Optional(1), value)

  expectEmpty(try? { () throws -> Int in throw SillyError.JazzHands }())
}

enum LifetimeError : ErrorProtocol {
  case MistakeOfALifetime(LifetimeTracked, yearsIncarcerated: Int)
}

ErrorProtocolTests.test("existential in lvalue") {
  expectEqual(0, LifetimeTracked.instances)
  do {
    var e: ErrorProtocol? = nil
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

