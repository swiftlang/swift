// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest


import Foundation
import CoreLocation
import Darwin

var ErrorBridgingTests = TestSuite("ErrorBridging")

var NoisyErrorLifeCount = 0
var NoisyErrorDeathCount = 0
var CanaryHandle = 0

protocol OtherProtocol {
  var otherProperty: String { get }
}

protocol OtherClassProtocol : class {
  var otherClassProperty: String { get }
}

class NoisyError : Error, OtherProtocol, OtherClassProtocol {
  init() { NoisyErrorLifeCount += 1 }
  deinit { NoisyErrorDeathCount += 1 }

  let _domain = "NoisyError"
  let _code = 123

  let otherProperty = "otherProperty"
  let otherClassProperty = "otherClassProperty"
}

@objc enum EnumError : Int, Error {
  case BadError = 9000
  case ReallyBadError = 9001
}

ErrorBridgingTests.test("NSError") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let ns = NSError(domain: "SomeDomain", code: 321, userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let e: Error = ns
    expectEqual(e._domain, "SomeDomain")
    expectEqual(e._code, 321)

    let ns2 = e as NSError
    expectTrue(ns === ns2)
    expectEqual(ns2._domain, "SomeDomain")
    expectEqual(ns2._code, 321)
  }
  expectEqual(NoisyErrorDeathCount, NoisyErrorLifeCount)
}

ErrorBridgingTests.test("NSCopying") {
  autoreleasepool {
    let orig = EnumError.ReallyBadError as NSError
    let copy = orig.copy() as! NSError
    expectEqual(orig, copy)
  }
}

func archiveAndUnarchiveObject<T: NSCoding>(
  _ object: T
) -> T?
where T: NSObject {
  let unarchiver = NSKeyedUnarchiver(forReadingWith:
    NSKeyedArchiver.archivedData(withRootObject: object)
  )
  unarchiver.requiresSecureCoding = true
  return unarchiver.decodeObject(of: T.self, forKey: "root")
}
ErrorBridgingTests.test("NSCoding") {
  autoreleasepool {
    let orig = EnumError.ReallyBadError as NSError
    let unarchived = archiveAndUnarchiveObject(orig)!
    expectEqual(orig, unarchived)
    expectTrue(type(of: unarchived) == NSError.self)
  }
}

ErrorBridgingTests.test("NSError-to-enum bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  let testURL = URL(string: "https://swift.org")!

  autoreleasepool {
    let underlyingError = CocoaError(.fileLockingError)
      as Error as NSError
    let ns = NSError(domain: NSCocoaErrorDomain,
                     code: NSFileNoSuchFileError,
                     userInfo: [
                       AnyHashable(NSFilePathErrorKey): "/dev/null",
                       AnyHashable(NSStringEncodingErrorKey): /*ASCII=*/1,
                       AnyHashable(NSUnderlyingErrorKey): underlyingError,
                       AnyHashable(NSURLErrorKey): testURL
                     ])

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
  
    let e: Error = ns

    let cocoaCode: Int?
    switch e {
    case let x as CocoaError:
      cocoaCode = x._code
      expectTrue(x.isFileError)
      expectEqual(x.code, CocoaError.fileNoSuchFileError)
    default:
      cocoaCode = nil
    }

    expectEqual(NSFileNoSuchFileError, cocoaCode)

    let cocoaCode2: Int? = (ns as? CocoaError)?._code
    expectEqual(NSFileNoSuchFileError, cocoaCode2)

    let isNoSuchFileError: Bool
    switch e {
    case CocoaError.fileNoSuchFileError:
      isNoSuchFileError = true
    default:
      isNoSuchFileError = false
    }

    expectTrue(isNoSuchFileError)

    // Check the contents of the error.
    let cocoaError = e as! CocoaError
    expectOptionalEqual("/dev/null", cocoaError.filePath)
    expectOptionalEqual(String.Encoding.ascii, cocoaError.stringEncoding)
    expectOptionalEqual(underlyingError, cocoaError.underlying.map { $0 as NSError })
    expectOptionalEqual(testURL, cocoaError.url)

    // URLError domain
    let nsURL = NSError(domain: NSURLErrorDomain,
                        code: NSURLErrorBadURL,
                        userInfo: [AnyHashable(NSURLErrorFailingURLErrorKey): testURL])
    let eURL: Error = nsURL
    let isBadURLError: Bool
    switch eURL {
    case URLError.badURL:
      isBadURLError = true
    default:
      isBadURLError = false
    }

    expectTrue(isBadURLError)

    let urlError = eURL as! URLError
    expectOptionalEqual(testURL, urlError.failingURL)
    expectNil(urlError.failureURLPeerTrust)

    // CoreLocation error domain
    let nsCL = NSError(domain: kCLErrorDomain,
                       code: CLError.headingFailure.rawValue,
                       userInfo: [AnyHashable(NSURLErrorKey): testURL])
    let eCL: Error = nsCL
    let isHeadingFailure: Bool
    switch eCL {
    case CLError.headingFailure:
      isHeadingFailure = true
    default:
      isHeadingFailure = false
    }

    let isCLError: Bool
    switch eCL {
    case let error as CLError:
      isCLError = true
      expectOptionalEqual(testURL, (error as NSError).userInfo[NSURLErrorKey as NSObject] as? URL)
      expectOptionalEqual(testURL, error.userInfo[NSURLErrorKey] as? URL)
    default:
      isCLError = false
    }

    expectTrue(isCLError)

    // NSPOSIXError domain
    let nsPOSIX = NSError(domain: NSPOSIXErrorDomain,
                          code: Int(EDEADLK),
                          userInfo: [:])
    let ePOSIX: Error = nsPOSIX
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
    let eMach: Error = nsMach
    let isMemoryFailure: Bool
    switch eMach {
    case MachError.memoryFailure:
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

struct StructError: Error {
  var _domain: String { return "StructError" }
  var _code: Int { return 4812 }
}

ErrorBridgingTests.test("Error-to-NSError bridging") {
  NoisyErrorLifeCount = 0
  NoisyErrorDeathCount = 0
  autoreleasepool {
    let e: Error = NoisyError()
    let ns = e as NSError
    let ns2 = e as NSError
    expectTrue(ns === ns2)
    expectEqual(ns._domain, "NoisyError")
    expectEqual(ns._code, 123)

    let e3: Error = ns
    expectEqual(e3._domain, "NoisyError")
    expectEqual(e3._code, 123)
    let ns3 = e3 as NSError
    expectTrue(ns === ns3)

    let nativeNS = NSError(domain: NSCocoaErrorDomain,
                           code: NSFileNoSuchFileError,
                           userInfo: nil)

    objc_setAssociatedObject(ns, &CanaryHandle, NoisyError(),
                             .OBJC_ASSOCIATION_RETAIN_NONATOMIC)

    let nativeE: Error = nativeNS
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

ErrorBridgingTests.test("enum-to-NSError round trip") {
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


ErrorBridgingTests.test("Thrown NSError identity is preserved") {
  do {
    let e = NSError(domain: "ClericalError", code: 219,
                    userInfo: [AnyHashable("yeah"): "yeah"])
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
                                userInfo: [AnyHashable("yeah"): "yeah"])
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
struct MyCustomizedError : Error {
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
  static var errorDomain: String {
    return "custom"
  }

  /// The error code within the given domain.
  var errorCode: Int {
    return code
  }

  /// The user-info dictionary.
  var errorUserInfo: [String : Any] {
    return [NSURLErrorKey : URL(string: "https://swift.org")!]
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

/// An error type that provides localization and recovery, but doesn't
/// customize NSError directly.
enum MySwiftCustomizedError : Error {
  case failed
  static var errorDescriptionCount = 0
}

extension MySwiftCustomizedError : LocalizedError {
  var errorDescription: String? {
    MySwiftCustomizedError.errorDescriptionCount =
      MySwiftCustomizedError.errorDescriptionCount + 1
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

extension MySwiftCustomizedError : RecoverableError {
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
  func attemptRecovery(fromError nsError: Error,
                       optionIndex recoveryOptionIndex: Int,
                       delegate: AnyObject?,
                       didRecoverSelector: Selector,
                       contextInfo: UnsafeMutableRawPointer?)

  @objc(attemptRecoveryFromError:optionIndex:)
  func attemptRecovery(fromError nsError: Error,
                       optionIndex recoveryOptionIndex: Int) -> Bool
}

class RecoveryDelegate {
  let expectedSuccess: Bool
  let expectedContextInfo: UnsafeMutableRawPointer?
  var called = false

  init(expectedSuccess: Bool,
       expectedContextInfo: UnsafeMutableRawPointer?) {
    self.expectedSuccess = expectedSuccess
    self.expectedContextInfo = expectedContextInfo
  }

  @objc func recover(success: Bool, contextInfo: UnsafeMutableRawPointer?) {
    expectEqual(expectedSuccess, success)
    expectEqual(expectedContextInfo, contextInfo)
    called = true
  }
}

/// Helper for testing a customized error.
func testCustomizedError(error: Error, nsError: NSError) {
  // LocalizedError
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectNil(nsError.userInfo[NSLocalizedDescriptionKey as NSObject])
    expectNil(nsError.userInfo[NSLocalizedFailureReasonErrorKey as NSObject])
    expectNil(nsError.userInfo[NSLocalizedRecoverySuggestionErrorKey as NSObject])
    expectNil(nsError.userInfo[NSHelpAnchorErrorKey as NSObject])
  } else {
    expectOptionalEqual("something went horribly wrong",
      nsError.userInfo[NSLocalizedDescriptionKey as NSObject] as? String)
    expectOptionalEqual("because someone wrote 'throw'",
      nsError.userInfo[NSLocalizedFailureReasonErrorKey as NSObject] as? String)
    expectOptionalEqual("delete the 'throw'",
      nsError.userInfo[NSLocalizedRecoverySuggestionErrorKey as NSObject] as? String)
    expectOptionalEqual("there is no help when writing tests",
      nsError.userInfo[NSHelpAnchorErrorKey as NSObject] as? String)
  }
  expectEqual("something went horribly wrong", error.localizedDescription)
  expectEqual("something went horribly wrong", nsError.localizedDescription)
  expectEqual("because someone wrote 'throw'", nsError.localizedFailureReason)
  expectEqual("delete the 'throw'", nsError.localizedRecoverySuggestion)
  expectEqual("there is no help when writing tests", nsError.helpAnchor)

  // RecoverableError
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectNil(nsError.userInfo[NSLocalizedRecoveryOptionsErrorKey as NSObject])
  } else {
    expectOptionalEqual(["Delete 'throw'", "Disable the test" ],
      nsError.userInfo[NSLocalizedRecoveryOptionsErrorKey as NSObject] as? [String])
  }
  expectOptionalEqual(["Delete 'throw'", "Disable the test" ],
    nsError.localizedRecoveryOptions)

  // Directly recover.
  let attempter: AnyObject
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectNil(nsError.userInfo[NSRecoveryAttempterErrorKey as NSObject])
    attempter = nsError.recoveryAttempter! as AnyObject
  } else {
    attempter = nsError.userInfo[NSRecoveryAttempterErrorKey as NSObject]! as AnyObject
  }
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

ErrorBridgingTests.test("Customizing NSError via protocols") {
  let error = MyCustomizedError(code: 12345)
  let nsError = error as NSError

  // CustomNSError
  expectEqual("custom", nsError.domain)
  expectEqual(12345, nsError.code)
  expectOptionalEqual(URL(string: "https://swift.org")!,
    nsError.userInfo[NSURLErrorKey as NSObject] as? URL)

  testCustomizedError(error: error, nsError: nsError)
}

ErrorBridgingTests.test("Customizing localization/recovery via protocols") {
  let error = MySwiftCustomizedError.failed
  let nsError = error as NSError
  testCustomizedError(error: error, nsError: nsError)
}

ErrorBridgingTests.test("Customizing localization/recovery laziness") {
  let countBefore = MySwiftCustomizedError.errorDescriptionCount
  let error = MySwiftCustomizedError.failed
  let nsError = error as NSError

  // RecoverableError
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectNil(nsError.userInfo[NSLocalizedRecoveryOptionsErrorKey as NSObject])
  } else {
    expectOptionalEqual(["Delete 'throw'", "Disable the test" ],
      nsError.userInfo[NSLocalizedRecoveryOptionsErrorKey as NSObject] as? [String])
  }
  expectOptionalEqual(["Delete 'throw'", "Disable the test" ],
    nsError.localizedRecoveryOptions)

  // None of the operations above should affect the count
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectEqual(countBefore, MySwiftCustomizedError.errorDescriptionCount)
  }

  // This one does affect the count.
  expectEqual("something went horribly wrong", error.localizedDescription)

  // Check that we did get a call to errorDescription.
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    expectEqual(countBefore+1, MySwiftCustomizedError.errorDescriptionCount)
  }
}

enum DefaultCustomizedError1 : CustomNSError {
  case bad
  case worse
}

enum DefaultCustomizedError2 : Int, CustomNSError {
  case bad = 7
  case worse = 13
}

enum DefaultCustomizedError3 : UInt, CustomNSError {
  case bad = 9
  case worse = 115

  static var errorDomain: String {
    return "customized3"
  }
}

enum DefaultCustomizedParent {
    enum ChildError: CustomNSError {
        case foo
    }
}

ErrorBridgingTests.test("Default-customized via CustomNSError") {
  expectEqual(1, (DefaultCustomizedError1.worse as NSError).code)
  expectEqual(13, (DefaultCustomizedError2.worse as NSError).code)
  expectEqual(115, (DefaultCustomizedError3.worse as NSError).code)
  expectEqual("main.DefaultCustomizedError1", (DefaultCustomizedError1.worse as NSError).domain)
  expectEqual("customized3", (DefaultCustomizedError3.worse as NSError).domain)
  expectEqual("main.DefaultCustomizedParent.ChildError", (DefaultCustomizedParent.ChildError.foo as NSError).domain)
}

class MyNSError : NSError {  }

ErrorBridgingTests.test("NSError subclass identity") {
  let myNSError: Error = MyNSError(domain: "MyNSError", code: 0, userInfo: [:])
  let nsError = myNSError as NSError
  expectTrue(type(of: nsError) == MyNSError.self)
}

ErrorBridgingTests.test("Wrapped NSError identity") {
  let nsError = NSError(domain: NSCocoaErrorDomain,
                   code: NSFileNoSuchFileError,
                   userInfo: [
                     AnyHashable(NSFilePathErrorKey) : "/dev/null",
                     AnyHashable(NSStringEncodingErrorKey): /*ASCII=*/1,
                   ])

  let error: Error = nsError
  let nsError2: NSError = error as NSError
  expectTrue(nsError === nsError2)

  // Extracting the NSError via the runtime.
  let cocoaErrorAny: Any = error as! CocoaError
  let nsError3: NSError = cocoaErrorAny as! NSError
  expectTrue(nsError === nsError3)

  if let cocoaErrorAny2: Any = error as? CocoaError {
    let nsError4: NSError = cocoaErrorAny2 as! NSError
    expectTrue(nsError === nsError4)
  } else {
    expectUnreachable()
  }

  // Extracting the NSError via direct call.
  let cocoaError = error as! CocoaError
  let nsError5: NSError = cocoaError as NSError
  expectTrue(nsError === nsError5)

  if let cocoaError2 = error as? CocoaError {
    let nsError6: NSError = cocoaError as NSError
    expectTrue(nsError === nsError6)
  } else {
    expectUnreachable()
  }
}

extension Error {
	func asNSError() -> NSError {
		return self as NSError
	}
}

func unconditionalCast<T>(_ x: Any, to: T.Type) -> T {
  return x as! T
}

func conditionalCast<T>(_ x: Any, to: T.Type) -> T? {
  return x as? T
}

// SR-1562
ErrorBridgingTests.test("Error archetype identity") {
  let myError = NSError(domain: "myErrorDomain", code: 0,
                        userInfo: [ AnyHashable("one") : 1 ])
  expectTrue(myError === myError.asNSError())

  expectTrue(unconditionalCast(myError, to: Error.self) as NSError
     === myError)
  expectTrue(conditionalCast(myError, to: Error.self)! as NSError
     === myError)

  let nsError = NSError(domain: NSCocoaErrorDomain,
                        code: NSFileNoSuchFileError,
                        userInfo: [
                          AnyHashable(NSFilePathErrorKey) : "/dev/null",
                          AnyHashable(NSStringEncodingErrorKey): /*ASCII=*/1,
                        ])
  let cocoaError = nsError as Error as! CocoaError
  expectTrue(cocoaError.asNSError() === nsError)
  expectTrue(unconditionalCast(cocoaError, to: Error.self) as NSError
    === nsError)
  expectTrue(conditionalCast(cocoaError, to: Error.self)! as NSError
    === nsError)
}

runAllTests()
