// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest


import Foundation
import CoreLocation
import Darwin

var ErrorProtocolBridgingTests = TestSuite("ErrorProtocolBridging")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0
var CanaryHandle = 0

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

@objc enum EnumError : Int, ErrorProtocol {
  case BadError = 9000
  case ReallyBadError = 9001
}

ErrorProtocolBridgingTests.test("NSError") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ns = NSError(domain: "SomeDomain", code: 321, userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let e: ErrorProtocol = ns
    expectEqual(e._domain, "SomeDomain")
    expectEqual(e._code, 321)

    let ns2 = e as NSError
    expectTrue(ns === ns2)
    expectEqual(ns2._domain, "SomeDomain")
    expectEqual(ns2._code, 321)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorProtocolBridgingTests.test("NSCopying") {
  autoreleasepool {
    let orig = EnumError.ReallyBadError as NSError
    let copy = orig.copy() as! NSError
    expectEqual(orig, copy)
  }
}

func archiveAndUnarchiveObject<T: NSCoding where T: NSObject>(
  _ object: T
) -> T? {
  let unarchiver = NSKeyedUnarchiver(forReadingWith:
    NSKeyedArchiver.archivedData(withRootObject: object)
  )
  unarchiver.requiresSecureCoding = true
  return unarchiver.decodeObjectOfClass(T.self, forKey: "root")
}
ErrorProtocolBridgingTests.test("NSCoding") {
  autoreleasepool {
    let orig = EnumError.ReallyBadError as NSError
    let unarchived = archiveAndUnarchiveObject(orig)!
    expectEqual(orig, unarchived)
    expectTrue(unarchived.dynamicType == NSError.self)
  }
}

ErrorProtocolBridgingTests.test("NSError-to-enum bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ns = NSError(domain: NSCocoaErrorDomain,
                     code: NSFileNoSuchFileError,
                     userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
  
    let e: ErrorProtocol = ns

    let cocoaCode: Int?
    switch e {
    case let x as NSCocoaError:
      cocoaCode = x._code
      expectTrue(x.isFileError)
      expectEqual(x, NSCocoaError.fileNoSuchFileError)
      expectEqual(x.hashValue, NSFileNoSuchFileError)
    default:
      cocoaCode = nil
    }

    expectEqual(cocoaCode, NSFileNoSuchFileError)

    let cocoaCode2: Int? = (ns as? NSCocoaError)?._code
    expectEqual(cocoaCode2, NSFileNoSuchFileError)

    let isNoSuchFileError: Bool
    switch e {
    case NSCocoaError.fileNoSuchFileError:
      isNoSuchFileError = true
    default:
      isNoSuchFileError = false
    }

    expectTrue(isNoSuchFileError)

    // NSURLError domain
    let nsURL = NSError(domain: NSURLErrorDomain,
                        code: NSURLErrorBadURL,
                        userInfo: nil)
    let eURL: ErrorProtocol = nsURL
    let isBadURLError: Bool
    switch eURL {
    case NSURLError.badURL:
      isBadURLError = true
    default:
      isBadURLError = false
    }

    expectTrue(isBadURLError)

    // CoreLocation error domain
    let nsCL = NSError(domain: kCLErrorDomain,
                       code: CLError.headingFailure.rawValue,
                       userInfo: [:])
    let eCL: ErrorProtocol = nsCL
    let isHeadingFailure: Bool
    switch eCL {
    case CLError.headingFailure:
      isHeadingFailure = true
    default:
      isHeadingFailure = false
    }

    expectTrue(isHeadingFailure)

    // NSPOSIXError domain
    let nsPOSIX = NSError(domain: NSPOSIXErrorDomain,
                          code: Int(EDEADLK),
                          userInfo: [:])
    let ePOSIX: ErrorProtocol = nsPOSIX
    let isDeadlock: Bool
    switch ePOSIX {
    case POSIXError.EDEADLK:
      isDeadlock = true
    default:
      isDeadlock = false
    }

    expectTrue(isDeadlock)

    // NSMachError domain
    let nsMach = NSError(domain: NSMachErrorDomain,
                         code: Int(KERN_MEMORY_FAILURE),
                         userInfo: [:])
    let eMach: ErrorProtocol = nsMach
    let isMemoryFailure: Bool
    switch eMach {
    case MachError.KERN_MEMORY_FAILURE:
      isMemoryFailure = true
    default:
      isMemoryFailure = false
    }

    expectTrue(isMemoryFailure)
  }
  
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

func opaqueUpcastToAny<T>(_ x: T) -> Any {
  return x
}

struct StructError: ErrorProtocol {
  var _domain: String { return "StructError" }
  var _code: Int { return 4812 }
}

ErrorProtocolBridgingTests.test("ErrorProtocol-to-NSError bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let e: ErrorProtocol = NoisyError()
    let ns = e as NSError
    let ns2 = e as NSError
    expectTrue(ns === ns2)
    expectEqual(ns._domain, "NoisyError")
    expectEqual(ns._code, 123)

    let e3: ErrorProtocol = ns
    expectEqual(e3._domain, "NoisyError")
    expectEqual(e3._code, 123)
    let ns3 = e3 as NSError
    expectTrue(ns === ns3)

    let nativeNS = NSError(domain: NSCocoaErrorDomain,
                           code: NSFileNoSuchFileError,
                           userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let nativeE: ErrorProtocol = nativeNS
    let nativeNS2 = nativeE as NSError
    expectTrue(nativeNS === nativeNS2)
    expectEqual(nativeNS2._domain, NSCocoaErrorDomain)
    expectEqual(nativeNS2._code, NSFileNoSuchFileError)

    let any: Any = NoisyError()
    let ns4 = any as! NSError
    expectEqual(ns4._domain, "NoisyError")
    expectEqual(ns4._code, 123)

    let ao: AnyObject = NoisyError()
    let ns5 = ao as! NSError
    expectEqual(ns5._domain, "NoisyError")
    expectEqual(ns5._code, 123)

    let any2: Any = StructError()
    let ns6 = any2 as! NSError
    expectEqual(ns6._domain, "StructError")
    expectEqual(ns6._code, 4812)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorProtocolBridgingTests.test("enum-to-NSError round trip") {
  autoreleasepool {
    // Emulate throwing an error from Objective-C.
    func throwNSError(_ error: EnumError) throws {
      throw NSError(domain: "main.EnumError", code: error.rawValue,
                    userInfo: [:])
    }

    var caughtError: Bool

    caughtError = false
    do {
      try throwNSError(.BadError)
      expectUnreachable()
    } catch let error as EnumError {
      expectEqual(.BadError, error)
      caughtError = true
    } catch _ {
      expectUnreachable()
    }
    expectTrue(caughtError)

    caughtError = false
    do {
      try throwNSError(.ReallyBadError)
      expectUnreachable()
    } catch EnumError.BadError {
      expectUnreachable()
    } catch EnumError.ReallyBadError {
      caughtError = true
    } catch _ {
      expectUnreachable()
    }
    expectTrue(caughtError)
  }
}

class SomeNSErrorSubclass: NSError {}


ErrorProtocolBridgingTests.test("Thrown NSError identity is preserved") {
  do {
    let e = NSError(domain: "ClericalError", code: 219,
                    userInfo: ["yeah": "yeah"])
    do {
      throw e
    } catch let e2 as NSError {
      expectTrue(e === e2)
      expectTrue(e2.userInfo["yeah"] as? String == "yeah")
    } catch {
      expectUnreachable()
    }
  }

  do {
    let f = SomeNSErrorSubclass(domain: "ClericalError", code: 219,
                                userInfo: ["yeah": "yeah"])
    do {
      throw f
    } catch let f2 as NSError {
      expectTrue(f === f2)
      expectTrue(f2.userInfo["yeah"] as? String == "yeah")
    } catch {
      expectUnreachable()
    }
  }
}

// Check errors customized via protocol.
struct MyCustomizedError : ErrorProtocol {
  let domain: String
  let code: Int
}

extension MyCustomizedError : LocalizedError {
  var errorDescription: String? {
    return NSLocalizedString("something went horribly wrong", comment: "")
  }

  var failureReason: String? {
    return NSLocalizedString("because someone wrote 'throw'", comment: "")
  }

  var recoverySuggestion: String? {
    return NSLocalizedString("delete the 'throw'", comment: "")
  }

  var helpAnchor: String? {
    return NSLocalizedString("there is no help when writing tests", comment: "")
  }
}

extension MyCustomizedError : CustomNSError {
  var errorDomain: String {
    return domain
  }

  /// The error code within the given domain.
  var errorCode: Int {
    return code
  }

  /// The user-info dictionary.
  var errorUserInfo: [String : AnyObject] {
    return [ NSURLErrorKey : URL(string: "https://swift.org")! as AnyObject]
  }
}

extension MyCustomizedError : RecoverableError {
  var recoveryOptions: [String] {
    return ["Delete 'throw'", "Disable the test" ]
  }

  func attemptRecovery(optionIndex recoveryOptionIndex: Int) -> Bool {
    return recoveryOptionIndex == 0
  }
}

/// Fake definition of the informal protocol
/// "NSErrorRecoveryAttempting" that we use to poke at the object
/// produced for a RecoverableError.
@objc protocol FakeNSErrorRecoveryAttempting {
  @objc(attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:)
  func attemptRecovery(fromError nsError: ErrorProtocol,
                       optionIndex recoveryOptionIndex: Int,
                       delegate: AnyObject?,
                       didRecoverSelector: Selector,
                       contextInfo: UnsafeMutablePointer<Void>?)

  @objc(attemptRecoveryFromError:optionIndex:)
  func attemptRecovery(fromError nsError: ErrorProtocol,
                       optionIndex recoveryOptionIndex: Int) -> Bool
}

class RecoveryDelegate {
  let expectedSuccess: Bool
  let expectedContextInfo: UnsafeMutablePointer<Void>?
  var called = false

  init(expectedSuccess: Bool,
       expectedContextInfo: UnsafeMutablePointer<Void>?) {
    self.expectedSuccess = expectedSuccess
    self.expectedContextInfo = expectedContextInfo
  }

  @objc func recover(success: Bool, contextInfo: UnsafeMutablePointer<Void>?) {
    expectEqual(expectedSuccess, success)
    expectEqual(expectedContextInfo, contextInfo)
    called = true
  }
}

ErrorProtocolBridgingTests.test("Customizing NSError via protocols") {
  let error = MyCustomizedError(domain: "custom", code: 12345)
  let nsError = error as NSError

  // CustomNSError
  expectEqual("custom", nsError.domain)
  expectEqual(12345, nsError.code)
  expectOptionalEqual(URL(string: "https://swift.org")!,
    nsError.userInfo[NSURLErrorKey] as? URL)

  // LocalizedError
  expectOptionalEqual("something went horribly wrong",
    nsError.userInfo[NSLocalizedDescriptionKey] as? String)
  expectOptionalEqual("because someone wrote 'throw'", 
    nsError.userInfo[NSLocalizedFailureReasonErrorKey] as? String)
  expectOptionalEqual("delete the 'throw'",
    nsError.userInfo[NSLocalizedRecoverySuggestionErrorKey] as? String)
  expectOptionalEqual("there is no help when writing tests",
    nsError.userInfo[NSHelpAnchorErrorKey] as? String)

  // RecoverableError
  expectOptionalEqual(["Delete 'throw'", "Disable the test" ],
    nsError.userInfo[NSLocalizedRecoveryOptionsErrorKey] as? [String])

  // Directly recover.
  let attempter = nsError.userInfo[NSRecoveryAttempterErrorKey]! 
  expectOptionalEqual(attempter.attemptRecovery(fromError: nsError,
                      optionIndex: 0),
    true)
  expectOptionalEqual(attempter.attemptRecovery(fromError: nsError,
                      optionIndex: 1),
    false)

  // Recover through delegate.
  let rd1 = RecoveryDelegate(expectedSuccess: true, expectedContextInfo: nil)
  expectEqual(false, rd1.called)
  attempter.attemptRecovery(
    fromError: nsError,
    optionIndex: 0,
    delegate: rd1,
    didRecoverSelector: #selector(RecoveryDelegate.recover(success:contextInfo:)),
    contextInfo: nil)
  expectEqual(true, rd1.called)

  let rd2 = RecoveryDelegate(expectedSuccess: false, expectedContextInfo: nil)
  expectEqual(false, rd2.called)
  attempter.attemptRecovery(
    fromError: nsError,
    optionIndex: 1,
    delegate: rd2,
    didRecoverSelector: #selector(RecoveryDelegate.recover(success:contextInfo:)),
    contextInfo: nil)
  expectEqual(true, rd2.called)
}

runAllTests()
