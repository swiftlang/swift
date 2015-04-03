// RUN: %target-run-simple-swift

import StdlibUnittest
import Foundation

var ErrorTypeTests = TestSuite("ErrorType")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0

protocol OtherProtocol {
  var otherProperty: String { get }
}

protocol OtherClassProtocol: class {
  var otherClassProperty: String { get }
}

class NoisyError: _ErrorType, OtherProtocol, OtherClassProtocol {
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
  autoreleasepool {
    let e: _ErrorType = NoisyError()

    expectEqual(e.domain, "NoisyError")
    expectEqual(e.code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorTypeTests.test("reflection") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ne = NoisyError()
    let e: _ErrorType = ne

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
  autoreleasepool {
    let ne = NoisyError()
    let e: _ErrorType = ne

    expectTrue(e as! NoisyError === ne)
    expectEqual((e as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual((e as! OtherProtocol).otherProperty, "otherProperty")

    let op: OtherProtocol = ne
    expectEqual((op as! _ErrorType).domain, "NoisyError")
    expectEqual((op as! _ErrorType).code, 123)

    let ocp: OtherClassProtocol = ne
    expectEqual((ocp as! _ErrorType).domain, "NoisyError")
    expectEqual((ocp as! _ErrorType).code, 123)

    // Do the same with rvalues, so we exercise the
    // take-on-success/destroy-on-failure paths.

    expectEqual(((NoisyError() as _ErrorType) as! NoisyError).domain, "NoisyError")
    expectEqual(((NoisyError() as _ErrorType) as! OtherClassProtocol).otherClassProperty, "otherClassProperty")
    expectEqual(((NoisyError() as _ErrorType) as! OtherProtocol).otherProperty, "otherProperty")

    expectEqual(((NoisyError() as OtherProtocol) as! _ErrorType).domain, "NoisyError")
    expectEqual(((NoisyError() as OtherProtocol) as! _ErrorType).code, 123)

    expectEqual(((NoisyError() as OtherClassProtocol) as! _ErrorType).domain, "NoisyError")
    expectEqual(((NoisyError() as OtherClassProtocol) as! _ErrorType).code, 123)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

var CanaryHandle = 0

ErrorTypeTests.test("NSError") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ns = NSError(domain: "SomeDomain", code: 321, userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let e: _ErrorType = ns
    expectEqual(e.domain, "SomeDomain")
    expectEqual(e.code, 321)

    let ns2 = e as! NSError
    expectTrue(ns === ns2)
    expectEqual(ns2.domain, "SomeDomain")
    expectEqual(ns2.code, 321)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorTypeTests.test("NSError-to-enum bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ns = NSError(domain: NSCocoaErrorDomain,
                     code: NSFileNoSuchFileError,
                     userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
  
    let e: _ErrorType = ns

    let cocoaCode: Int?
    switch e {
    case let x as _NSCocoaError:
      cocoaCode = x.code
    default:
      cocoaCode = nil
    }

    expectEqual(cocoaCode, NSFileNoSuchFileError)

    let isNoSuchFileError: Bool
    switch e {
    case _NSCocoaError.NSFileNoSuchFileError:
      isNoSuchFileError = true
    default:
      isNoSuchFileError = false
    }

    expectTrue(isNoSuchFileError)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

// TODO: Should use this via language-level 'as NSError' coercion
@asmname("swift_becomeNSError")
public func swift_becomeNSError(e: _ErrorType) -> NSError

ErrorTypeTests.test("ErrorType-to-NSError bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let e: _ErrorType = NoisyError()
    let ns = swift_becomeNSError(e)
    let ns2 = swift_becomeNSError(e)
    expectTrue(ns === ns2)
    expectEqual(ns.domain, "NoisyError")
    expectEqual(ns.code, 123)

    let e3: _ErrorType = ns
    expectEqual(e3.domain, "NoisyError")
    expectEqual(e3.code, 123)
    let ns3 = swift_becomeNSError(e3)
    expectTrue(ns === ns3)

    let nativeNS = NSError(domain: NSCocoaErrorDomain,
                           code: NSFileNoSuchFileError,
                           userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let nativeE: _ErrorType = nativeNS
    let nativeNS2 = swift_becomeNSError(nativeE)
    expectTrue(nativeNS === nativeNS2)
    expectEqual(nativeNS2.domain, NSCocoaErrorDomain)
    expectEqual(nativeNS2.code, NSFileNoSuchFileError)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

runAllTests()

