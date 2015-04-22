// RUN: %target-run-simple-swift
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var ErrorTypeBridgingTests = TestSuite("ErrorTypeBridging")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0
var CanaryHandle = 0

protocol OtherProtocol {
  var otherProperty: String { get }
}

protocol OtherClassProtocol : class {
  var otherClassProperty: String { get }
}

class NoisyError : _ErrorType, OtherProtocol, OtherClassProtocol {
  init() { ++NoisyErrorLifeCount }
  deinit { ++NoisyErrorDeathCount }

  let domain = "NoisyError"
  let code = 123

  let otherProperty = "otherProperty"
  let otherClassProperty = "otherClassProperty"
}

ErrorTypeBridgingTests.test("NSError") {
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

ErrorTypeBridgingTests.test("NSError-to-enum bridging") {
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

    let cocoaCode2: Int? = (ns as? _NSCocoaError)?.code
    expectEqual(cocoaCode2, NSFileNoSuchFileError)

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

func opaqueUpcastToAny<T>(x: T) -> Any {
  return x
}

struct StructError: _ErrorType {
  var domain: String { return "StructError" }
  var code: Int { return 4812 }
}

ErrorTypeBridgingTests.test("ErrorType-to-NSError bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let e: _ErrorType = NoisyError()
    let ns = e as NSError
    let ns2 = e as NSError
    expectTrue(ns === ns2)
    expectEqual(ns.domain, "NoisyError")
    expectEqual(ns.code, 123)

    let e3: _ErrorType = ns
    expectEqual(e3.domain, "NoisyError")
    expectEqual(e3.code, 123)
    let ns3 = e3 as NSError
    expectTrue(ns === ns3)

    let nativeNS = NSError(domain: NSCocoaErrorDomain,
                           code: NSFileNoSuchFileError,
                           userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let nativeE: _ErrorType = nativeNS
    let nativeNS2 = nativeE as NSError
    expectTrue(nativeNS === nativeNS2)
    expectEqual(nativeNS2.domain, NSCocoaErrorDomain)
    expectEqual(nativeNS2.code, NSFileNoSuchFileError)

    /* TODO: Because of rdar://problem/20507075, we can't support rc-identity-
     * changing dynamic casts between class types.
    let any: Any = NoisyError()
    let ns3 = any as! NSError
    expectEqual(ns3.domain, "NoisyError")
    expectEqual(ns3.code, 123)

    let ao: AnyObject = NoisyError()
    let ns4 = ao as! NSError
    expectEqual(ns4.domain, "NoisyError")
    expectEqual(ns4.code, 123)
     */

    let any2: Any = StructError()
    let ns5 = any2 as! NSError
    expectEqual(ns5.domain, "StructError")
    expectEqual(ns5.code, 4812)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

runAllTests()
