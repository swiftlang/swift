import CoreFoundation
import Darwin

// NSError and CFError conform to the standard ErrorType protocol. Compiler
// magic allows this to be done as a "toll-free" conversion when an NSError
// or CFError is used as an ErrorType existential.

extension NSError : ErrorType {
  public var _domain: String { return domain }
  public var _code: Int { return code }
}

extension CFError : ErrorType {
  public var _domain: String {
    return CFErrorGetDomain(self) as String
  }

  public var _code: Int {
    return CFErrorGetCode(self)
  }
}

// An error value to use when an Objective-C API indicates error
// but produces a nil error object.
public enum _GenericObjCError : ErrorType {
  case NilError
}

/// An internal protocol to represent Swift error enums that map to standard
/// Cocoa NSError domains.
public protocol _ObjectiveCBridgeableErrorType : ErrorType {
  /// Produce a value of the error type corresponding to the given NSError,
  /// or return nil if it cannot be bridged.
  init?(_bridgedNSError: NSError)
}

/// A hook for the runtime to use _ObjectiveCBridgeableErrorType in order to
/// attempt an "errorTypeValue as? SomeError" cast.
///
/// If the bridge succeeds, the bridged value is written to the uninitialized
/// memory pointed to by 'out', and true is returned. Otherwise, 'out' is
/// left uninitialized, and false is returned.
@warn_unused_result
@_silgen_name("swift_stdlib_bridgeNSErrorToErrorType")
public func _stdlib_bridgeNSErrorToErrorType<
  T : _ObjectiveCBridgeableErrorType
>(error: NSError, out: UnsafeMutablePointer<T>) -> Bool {
  if let bridged = T(_bridgedNSError: error) {
    out.initialize(bridged)
    return true
  } else {
    return false
  }
}

/// Helper protocol for _BridgedNSError, which used to provide
/// default implementations.
public protocol __BridgedNSError : RawRepresentable, ErrorType {
  static var _NSErrorDomain: String { get }
}

// Allow two bridged NSError types to be compared.
@warn_unused_result
public func ==<T: __BridgedNSError where T.RawValue: SignedIntegerType>(
  lhs: T,
  rhs: T
) -> Bool {
  return lhs.rawValue.toIntMax() == rhs.rawValue.toIntMax()
}

public extension __BridgedNSError where RawValue: SignedIntegerType {
  public final var _domain: String { return Self._NSErrorDomain }
  public final var _code: Int { return Int(rawValue.toIntMax()) }

  public init?(rawValue: RawValue) {
    self = unsafeBitCast(rawValue, Self.self)
  }

  public init?(_bridgedNSError: NSError) {
    if _bridgedNSError.domain != Self._NSErrorDomain {
      return nil
    }

    self.init(rawValue: RawValue(IntMax(_bridgedNSError.code)))
  }

  public final var hashValue: Int { return _code }
}

// Allow two bridged NSError types to be compared.
@warn_unused_result
public func ==<T: __BridgedNSError where T.RawValue: UnsignedIntegerType>(
  lhs: T,
  rhs: T
) -> Bool {
  return lhs.rawValue.toUIntMax() == rhs.rawValue.toUIntMax()
}


public extension __BridgedNSError where RawValue: UnsignedIntegerType {
  public final var _domain: String { return Self._NSErrorDomain }
  public final var _code: Int {
    return Int(bitPattern: UInt(rawValue.toUIntMax()))
  }

  public init?(rawValue: RawValue) {
    self = unsafeBitCast(rawValue, Self.self)
  }

  public init?(_bridgedNSError: NSError) {
    if _bridgedNSError.domain != Self._NSErrorDomain {
      return nil
    }

    self.init(rawValue: RawValue(UIntMax(UInt(_bridgedNSError.code))))
  }

  public final var hashValue: Int { return _code }
}

/// Describes a raw representable type that is bridged to a particular
/// NSError domain.
///
/// This protocol is used primarily to generate the conformance to
/// _ObjectiveCBridgeableErrorType for such an enum.
public protocol _BridgedNSError : __BridgedNSError,
                                  _ObjectiveCBridgeableErrorType,
                                  Hashable {
  /// The NSError domain to which this type is bridged.
  static var _NSErrorDomain: String { get }
}

/// Enumeration that describes the error codes within the Cocoa error
/// domain.
public struct NSCocoaError : RawRepresentable, _BridgedNSError {
  public let rawValue: Int

  public init(rawValue: Int) {
    self.rawValue = rawValue
  }

  public static var _NSErrorDomain: String { return NSCocoaErrorDomain }
}

@warn_unused_result
public func ~=(match: NSCocoaError, error: ErrorType) -> Bool {
  guard let cocoaError = error as? NSCocoaError else { return false }
  return match.rawValue == cocoaError.rawValue
}

public extension NSCocoaError {
  @swift3_migration(renamed="fileNoSuchFileError")
  public static var FileNoSuchFileError: NSCocoaError {
    return NSCocoaError(rawValue: 4)
  }
  @swift3_migration(renamed="fileLockingError")
  public static var FileLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 255)
  }
  @swift3_migration(renamed="fileReadUnknownError")
  public static var FileReadUnknownError: NSCocoaError {
    return NSCocoaError(rawValue: 256)
  }
  @swift3_migration(renamed="fileReadNoPermissionError")
  public static var FileReadNoPermissionError: NSCocoaError {
    return NSCocoaError(rawValue: 257)
  }
  @swift3_migration(renamed="fileReadInvalidFileNameError")
  public static var FileReadInvalidFileNameError: NSCocoaError {
    return NSCocoaError(rawValue: 258)
  }
  @swift3_migration(renamed="fileReadCorruptFileError")
  public static var FileReadCorruptFileError: NSCocoaError {
    return NSCocoaError(rawValue: 259)
  }
  @swift3_migration(renamed="fileReadNoSuchFileError")
  public static var FileReadNoSuchFileError: NSCocoaError {
    return NSCocoaError(rawValue: 260)
  }
  @swift3_migration(renamed="fileReadInapplicableStringEncodingError")
  public static var FileReadInapplicableStringEncodingError: NSCocoaError {
    return NSCocoaError(rawValue: 261)
  }
  @swift3_migration(renamed="fileReadUnsupportedSchemeError")
  public static var FileReadUnsupportedSchemeError: NSCocoaError {
    return NSCocoaError(rawValue: 262)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="fileReadTooLargeError")
  public static var FileReadTooLargeError: NSCocoaError {
    return NSCocoaError(rawValue: 263)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="fileReadUnknownStringEncodingError")
  public static var FileReadUnknownStringEncodingError: NSCocoaError {
    return NSCocoaError(rawValue: 264)
  }

  @swift3_migration(renamed="fileWriteUnknownError")
  public static var FileWriteUnknownError: NSCocoaError {
    return NSCocoaError(rawValue: 512)
  }
  @swift3_migration(renamed="fileWriteNoPermissionError")
  public static var FileWriteNoPermissionError: NSCocoaError {
    return NSCocoaError(rawValue: 513)
  }
  @swift3_migration(renamed="fileWriteInvalidFileNameError")
  public static var FileWriteInvalidFileNameError: NSCocoaError {
    return NSCocoaError(rawValue: 514)
  }

  @available(OSX, introduced=10.7) @available(iOS, introduced=5.0)
  @swift3_migration(renamed="fileWriteFileExistsError")
  public static var FileWriteFileExistsError: NSCocoaError {
    return NSCocoaError(rawValue: 516)
  }

  @swift3_migration(renamed="fileWriteInapplicableStringEncodingError")
  public static var FileWriteInapplicableStringEncodingError: NSCocoaError {
    return NSCocoaError(rawValue: 517)
  }
  @swift3_migration(renamed="fileWriteUnsupportedSchemeError")
  public static var FileWriteUnsupportedSchemeError: NSCocoaError {
    return NSCocoaError(rawValue: 518)
  }
  @swift3_migration(renamed="fileWriteOutOfSpaceError")
  public static var FileWriteOutOfSpaceError: NSCocoaError {
    return NSCocoaError(rawValue: 640)
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  @swift3_migration(renamed="fileWriteVolumeReadOnlyError")
  public static var FileWriteVolumeReadOnlyError: NSCocoaError {
    return NSCocoaError(rawValue: 642)
  }

  @available(OSX, introduced=10.11) @available(iOS, unavailable)
  @swift3_migration(renamed="fileManagerUnmountUnknownError")
  public static var FileManagerUnmountUnknownError: NSCocoaError {
    return NSCocoaError(rawValue: 768)
  }

  @available(OSX, introduced=10.11) @available(iOS, unavailable)
  @swift3_migration(renamed="fileManagerUnmountBusyError")
  public static var FileManagerUnmountBusyError: NSCocoaError {
    return NSCocoaError(rawValue: 769)
  }

  @swift3_migration(renamed="keyValueValidationError")
  public static var KeyValueValidationError: NSCocoaError {
    return NSCocoaError(rawValue: 1024)
  }
  @swift3_migration(renamed="formattingError")
  public static var FormattingError: NSCocoaError {
    return NSCocoaError(rawValue: 2048)
  }
  @swift3_migration(renamed="userCancelledError")
  public static var UserCancelledError: NSCocoaError {
    return NSCocoaError(rawValue: 3072)
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  @swift3_migration(renamed="featureUnsupportedError")
  public static var FeatureUnsupportedError: NSCocoaError {
    return NSCocoaError(rawValue: 3328)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="executableNotLoadableError")
  public static var ExecutableNotLoadableError: NSCocoaError {
    return NSCocoaError(rawValue: 3584)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="executableArchitectureMismatchError")
  public static var ExecutableArchitectureMismatchError: NSCocoaError {
    return NSCocoaError(rawValue: 3585)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="executableRuntimeMismatchError")
  public static var ExecutableRuntimeMismatchError: NSCocoaError {
    return NSCocoaError(rawValue: 3586)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="executableLoadError")
  public static var ExecutableLoadError: NSCocoaError {
    return NSCocoaError(rawValue: 3587)
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  @swift3_migration(renamed="executableLinkError")
  public static var ExecutableLinkError: NSCocoaError {
    return NSCocoaError(rawValue: 3588)
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  @swift3_migration(renamed="propertyListReadCorruptError")
  public static var PropertyListReadCorruptError: NSCocoaError {
    return NSCocoaError(rawValue: 3840)
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  @swift3_migration(renamed="propertyListReadUnknownVersionError")
  public static var PropertyListReadUnknownVersionError: NSCocoaError {
    return NSCocoaError(rawValue: 3841)
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  @swift3_migration(renamed="propertyListReadStreamError")
  public static var PropertyListReadStreamError: NSCocoaError {
    return NSCocoaError(rawValue: 3842)
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  @swift3_migration(renamed="propertyListWriteStreamError")
  public static var PropertyListWriteStreamError: NSCocoaError {
    return NSCocoaError(rawValue: 3851)
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="propertyListWriteInvalidError")
  public static var PropertyListWriteInvalidError: NSCocoaError {
    return NSCocoaError(rawValue: 3852)
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  @swift3_migration(renamed="xpcConnectionInterrupted")
  public static var XPCConnectionInterrupted: NSCocoaError {
    return NSCocoaError(rawValue: 4097)
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  @swift3_migration(renamed="xpcConnectionInvalid")
  public static var XPCConnectionInvalid: NSCocoaError {
    return NSCocoaError(rawValue: 4099)
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  @swift3_migration(renamed="xpcConnectionReplyInvalid")
  public static var XPCConnectionReplyInvalid: NSCocoaError {
    return NSCocoaError(rawValue: 4101)
  }

  @available(OSX, introduced=10.9) @available(iOS, introduced=7.0)
  @swift3_migration(renamed="ubiquitousFileUnavailableError")
  public static var UbiquitousFileUnavailableError: NSCocoaError {
    return NSCocoaError(rawValue: 4353)
  }

  @available(OSX, introduced=10.9) @available(iOS, introduced=7.0)
  @swift3_migration(renamed="ubiquitousFileNotUploadedDueToQuotaError")
  public static var UbiquitousFileNotUploadedDueToQuotaError: NSCocoaError {
    return NSCocoaError(rawValue: 4354)
  }

  @available(OSX, introduced=10.9) @available(iOS, introduced=7.0)
  @swift3_migration(renamed="ubiquitousFileUbiquityServerNotAvailable")
  public static var UbiquitousFileUbiquityServerNotAvailable: NSCocoaError {
    return NSCocoaError(rawValue: 4355)
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="userActivityHandoffFailedError")
  public static var UserActivityHandoffFailedError: NSCocoaError {
    return NSCocoaError(rawValue: 4608)
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="userActivityConnectionUnavailableError")
  public static var UserActivityConnectionUnavailableError: NSCocoaError {
    return NSCocoaError(rawValue: 4609)
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="userActivityRemoteApplicationTimedOutError")
  public static var UserActivityRemoteApplicationTimedOutError: NSCocoaError {
    return NSCocoaError(rawValue: 4610)
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="userActivityHandoffUserInfoTooLargeError")
  public static var UserActivityHandoffUserInfoTooLargeError: NSCocoaError {
    return NSCocoaError(rawValue: 4611)
  }

  @available(OSX, introduced=10.11) @available(iOS, introduced=9.0)
  @swift3_migration(renamed="coderReadCorruptError")
  public static var CoderReadCorruptError: NSCocoaError {
    return NSCocoaError(rawValue: 4864)
  }

  @available(OSX, introduced=10.11) @available(iOS, introduced=9.0)
  @swift3_migration(renamed="coderValueNotFoundError")
  public static var CoderValueNotFoundError: NSCocoaError {
    return NSCocoaError(rawValue: 4865)
  }


  @available(OSX, introduced=10.11) @available(iOS, introduced=9.0)
  public var isCoderError: Bool {
    return rawValue >= 4864 && rawValue <= 4991
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  public var isExecutableError: Bool {
    return rawValue >= 3584 && rawValue <= 3839
  }

  public var isFileError: Bool {
    return rawValue >= 0 && rawValue <= 1023
  }

  public var isFormattingError: Bool {
    return rawValue >= 2048 && rawValue <= 2559
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  public var isPropertyListError: Bool {
    return rawValue >= 3840 && rawValue <= 4095
  }

  @available(OSX, introduced=10.9) @available(iOS, introduced=7.0)
  public var isUbiquitousFileError: Bool {
    return rawValue >= 4352 && rawValue <= 4607
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  public var isUserActivityError: Bool {
    return rawValue >= 4608 && rawValue <= 4863
  }

  public var isValidationError: Bool {
    return rawValue >= 1024 && rawValue <= 2047
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  public var isXPCConnectionError: Bool {
    return rawValue >= 4096 && rawValue <= 4224
  }
}

/// Enumeration that describes the error codes within the NSURL error
/// domain.
@objc public enum NSURLError : Int, _BridgedNSError {
  @swift3_migration(renamed="unknown")
  case Unknown = -1
  @swift3_migration(renamed="cancelled")
  case Cancelled = -999
  @swift3_migration(renamed="badURL")
  case BadURL = -1000
  @swift3_migration(renamed="timedOut")
  case TimedOut = -1001
  @swift3_migration(renamed="unsupportedURL")
  case UnsupportedURL = -1002
  @swift3_migration(renamed="cannotFindHost")
  case CannotFindHost = -1003
  @swift3_migration(renamed="cannotConnectToHost")
  case CannotConnectToHost = -1004
  @swift3_migration(renamed="networkConnectionLost")
  case NetworkConnectionLost = -1005
  @swift3_migration(renamed="dnsLookupFailed")
  case DNSLookupFailed = -1006
  @swift3_migration(renamed="httpTooManyRedirects")
  case HTTPTooManyRedirects = -1007
  @swift3_migration(renamed="resourceUnavailable")
  case ResourceUnavailable = -1008
  @swift3_migration(renamed="notConnectedToInternet")
  case NotConnectedToInternet = -1009
  @swift3_migration(renamed="redirectToNonExistentLocation")
  case RedirectToNonExistentLocation = -1010
  @swift3_migration(renamed="badServerResponse")
  case BadServerResponse = -1011
  @swift3_migration(renamed="userCancelledAuthentication")
  case UserCancelledAuthentication = -1012
  @swift3_migration(renamed="userAuthenticationRequired")
  case UserAuthenticationRequired = -1013
  @swift3_migration(renamed="zeroByteResource")
  case ZeroByteResource = -1014
  @swift3_migration(renamed="cannotDecodeRawData")
  case CannotDecodeRawData = -1015
  @swift3_migration(renamed="cannotDecodeContentData")
  case CannotDecodeContentData = -1016
  @swift3_migration(renamed="cannotParseResponse")
  case CannotParseResponse = -1017
  @swift3_migration(renamed="fileDoesNotExist")
  case FileDoesNotExist = -1100
  @swift3_migration(renamed="fileIsDirectory")
  case FileIsDirectory = -1101
  @swift3_migration(renamed="noPermissionsToReadFile")
  case NoPermissionsToReadFile = -1102
  @swift3_migration(renamed="secureConnectionFailed")
  case SecureConnectionFailed = -1200
  @swift3_migration(renamed="serverCertificateHasBadDate")
  case ServerCertificateHasBadDate = -1201
  @swift3_migration(renamed="serverCertificateUntrusted")
  case ServerCertificateUntrusted = -1202
  @swift3_migration(renamed="serverCertificateHasUnknownRoot")
  case ServerCertificateHasUnknownRoot = -1203
  @swift3_migration(renamed="serverCertificateNotYetValid")
  case ServerCertificateNotYetValid = -1204
  @swift3_migration(renamed="clientCertificateRejected")
  case ClientCertificateRejected = -1205
  @swift3_migration(renamed="clientCertificateRequired")
  case ClientCertificateRequired = -1206
  @swift3_migration(renamed="cannotLoadFromNetwork")
  case CannotLoadFromNetwork = -2000
  @swift3_migration(renamed="cannotCreateFile")
  case CannotCreateFile = -3000
  @swift3_migration(renamed="cannotOpenFile")
  case CannotOpenFile = -3001
  @swift3_migration(renamed="cannotCloseFile")
  case CannotCloseFile = -3002
  @swift3_migration(renamed="cannotWriteToFile")
  case CannotWriteToFile = -3003
  @swift3_migration(renamed="cannotRemoveFile")
  case CannotRemoveFile = -3004
  @swift3_migration(renamed="cannotMoveFile")
  case CannotMoveFile = -3005
  @swift3_migration(renamed="downloadDecodingFailedMidStream")
  case DownloadDecodingFailedMidStream = -3006
  @swift3_migration(renamed="downloadDecodingFailedToComplete")
  case DownloadDecodingFailedToComplete = -3007

  @available(OSX, introduced=10.7) @available(iOS, introduced=3.0)
  @swift3_migration(renamed="internationalRoamingOff")
  case InternationalRoamingOff = -1018

  @available(OSX, introduced=10.7) @available(iOS, introduced=3.0)
  @swift3_migration(renamed="callIsActive")
  case CallIsActive = -1019

  @available(OSX, introduced=10.7) @available(iOS, introduced=3.0)
  @swift3_migration(renamed="dataNotAllowed")
  case DataNotAllowed = -1020

  @available(OSX, introduced=10.7) @available(iOS, introduced=3.0)
  @swift3_migration(renamed="requestBodyStreamExhausted")
  case RequestBodyStreamExhausted = -1021

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="backgroundSessionRequiresSharedContainer")
  case BackgroundSessionRequiresSharedContainer = -995

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="backgroundSessionInUseByAnotherProcess")
  case BackgroundSessionInUseByAnotherProcess = -996

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  @swift3_migration(renamed="backgroundSessionWasDisconnected")
  case BackgroundSessionWasDisconnected = -997

  public static var _NSErrorDomain: String { return NSURLErrorDomain }
}

extension POSIXError : _BridgedNSError {
  public static var _NSErrorDomain: String { return NSPOSIXErrorDomain }
}

extension MachError : _BridgedNSError {
  public static var _NSErrorDomain: String { return NSMachErrorDomain }
}
