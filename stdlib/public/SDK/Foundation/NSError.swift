//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CoreFoundation
import Darwin

/// Describes an error that provides localized messages describing why
/// an error occurred and provides more information about the error.
public protocol LocalizedError : ErrorProtocol {
  /// A localized message describing what error occurred.
  var errorDescription: String? { get }

  /// A localized message describing the reason for the failure.
  var failureReason: String? { get }

  /// A localized message describing how one might recover from the failure.
  var recoverySuggestion: String? { get }

  /// A localized message providing "help" text if the user requests help.
  var helpAnchor: String? { get }
}

public extension LocalizedError {
  var errorDescription: String? { return nil }
  var failureReason: String? { return nil }
  var recoverySuggestion: String? { return nil }
  var helpAnchor: String? { return nil }
}

@_silgen_name("NS_Swift_performErrorRecoverySelector")
internal func NS_Swift_performErrorRecoverySelector(
  delegate: AnyObject?,
  selector: Selector,
  success: ObjCBool,
  contextInfo: UnsafeMutablePointer<Void>?)

/// Class that implements the informal protocol
/// NSErrorRecoveryAttempting, which is used by NSError when it
/// attempts recovery from an error.
class _NSErrorRecoveryAttempter {
  // FIXME: If we could meaningfully cast the nsError back to RecoverableError,
  // we wouldn't need to capture this and could use the user-info
  // domain providers even for recoverable errors.
  let error: RecoverableError

  init(error: RecoverableError) {
    self.error = error
  }

  @objc(attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:)
  func attemptRecovery(fromError nsError: NSError,
                       optionIndex recoveryOptionIndex: Int,
                       delegate: AnyObject?,
                       didRecoverSelector: Selector,
                       contextInfo: UnsafeMutablePointer<Void>?) {
    error.attemptRecovery(optionIndex: recoveryOptionIndex) { success in
      NS_Swift_performErrorRecoverySelector(
        delegate: delegate,
        selector: didRecoverSelector,
        success: ObjCBool(success),
        contextInfo: contextInfo)
    }
  }

  @objc(attemptRecoveryFromError:optionIndex:)
  func attemptRecovery(fromError nsError: NSError,
                       optionIndex recoveryOptionIndex: Int) -> Bool {
    return error.attemptRecovery(optionIndex: recoveryOptionIndex)
  }
}

/// Describes an error that may be recoverably by presenting several
/// potential recovery options to the user.
public protocol RecoverableError : ErrorProtocol {
  /// Provides a set of possible recovery options to present to the user.
  var recoveryOptions: [String] { get }

  /// Attempt to recover from this error when the user selected the
  /// option at the given index. This routine must call resultHandler and
  /// indicate whether recovery was successful (or not).
  ///
  /// This entry point is used for recovery of errors handled at a
  /// "document" granularity, that do not affect the entire
  /// application.
  func attemptRecovery(optionIndex recoveryOptionIndex: Int,
                       andThen resultHandler: (recovered: Bool) -> Void)

  /// Attempt to recover from this error when the user selected the
  /// option at the given index. Returns true to indicate
  /// successful recovery, and false otherwise.
  ///
  /// This entry point is used for recovery of errors handled at
  /// the "application" granularity, where nothing else in the
  /// application can proceed until the attmpted error recovery
  /// completes.
  func attemptRecovery(optionIndex recoveryOptionIndex: Int) -> Bool
}

public extension RecoverableError {
  /// By default, implements document-modal recovery via application-model
  /// recovery.
  func attemptRecovery(optionIndex recoveryOptionIndex: Int,
                       andThen resultHandler: (recovered: Bool) -> Void) {
    resultHandler(recovered: attemptRecovery(optionIndex: recoveryOptionIndex))
  }
}

/// Describes an error type that specifically provides a domain, code,
/// and user-info dictionary.
public protocol CustomNSError : ErrorProtocol {
  /// The domain of the error.
  static var errorDomain: String { get }

  /// The error code within the given domain.
  var errorCode: Int { get }

  /// The user-info dictionary.
  var errorUserInfo: [String : AnyObject] { get }
}

public extension ErrorProtocol where Self : CustomNSError {
  /// Default implementation for customized NSErrors.
  var _domain: String { return Self.errorDomain }

  /// Default implementation for customized NSErrors.
  var _code: Int { return self.errorCode }
}

public extension ErrorProtocol {
  /// Retrieve the localized description for this error.
  var localizedDescription: String {
    return (self as! NSError).localizedDescription
  }
}

/// Retrieve the default userInfo dictionary for a given error.
@_silgen_name("swift_Foundation_getErrorDefaultUserInfo")
public func _swift_Foundation_getErrorDefaultUserInfo(_ error: ErrorProtocol)
  -> AnyObject? {
  // If the OS supports user info value providers, use those
  // to lazily populate the user-info dictionary for this domain.
  if #available(OSX 10.11, iOS 9.0, tvOS 9.0, watchOS 2.0, *) {
    // FIXME: This is not implementable until we can recover the
    // original error from an NSError.
  }

  // Populate the user-info dictionary 
  var result: [String : AnyObject]

  // Initialize with custom user-info.
  if let customNSError = error as? CustomNSError {
    result = customNSError.errorUserInfo
  } else {
    result = [:]
  }

  if let localizedError = error as? LocalizedError {
    if let description = localizedError.errorDescription {
      result[NSLocalizedDescriptionKey] = description as AnyObject
    }
    
    if let reason = localizedError.failureReason {
      result[NSLocalizedFailureReasonErrorKey] = reason as AnyObject
    }
    
    if let suggestion = localizedError.recoverySuggestion {   
      result[NSLocalizedRecoverySuggestionErrorKey] = suggestion as AnyObject
    }
    
    if let helpAnchor = localizedError.helpAnchor {   
      result[NSHelpAnchorErrorKey] = helpAnchor as AnyObject
    }
  }

  if let recoverableError = error as? RecoverableError {
    result[NSLocalizedRecoveryOptionsErrorKey] =
      recoverableError.recoveryOptions as AnyObject
    result[NSRecoveryAttempterErrorKey] =
      _NSErrorRecoveryAttempter(error: recoverableError)
  }

  return result as AnyObject
}

// NSError and CFError conform to the standard ErrorProtocol protocol. Compiler
// magic allows this to be done as a "toll-free" conversion when an NSError
// or CFError is used as an ErrorProtocol existential.

extension NSError : ErrorProtocol {
  public var _domain: String { return domain }
  public var _code: Int { return code }
  public var _userInfo: AnyObject? { return userInfo as AnyObject }
}

extension CFError : ErrorProtocol {
  public var _domain: String {
    return CFErrorGetDomain(self) as String
  }

  public var _code: Int {
    return CFErrorGetCode(self)
  }

  public var _userInfo: AnyObject? {
    return CFErrorCopyUserInfo(self) as AnyObject?
  }
}

// An error value to use when an Objective-C API indicates error
// but produces a nil error object.
public enum _GenericObjCError : ErrorProtocol {
  case nilError
}

/// An internal protocol to represent Swift error enums that map to standard
/// Cocoa NSError domains.
public protocol _ObjectiveCBridgeableErrorProtocol : ErrorProtocol {
  /// Produce a value of the error type corresponding to the given NSError,
  /// or return nil if it cannot be bridged.
  init?(_bridgedNSError: NSError)
}

/// A hook for the runtime to use _ObjectiveCBridgeableErrorProtocol in order to
/// attempt an "errorTypeValue as? SomeError" cast.
///
/// If the bridge succeeds, the bridged value is written to the uninitialized
/// memory pointed to by 'out', and true is returned. Otherwise, 'out' is
/// left uninitialized, and false is returned.
@_silgen_name("swift_stdlib_bridgeNSErrorToErrorProtocol")
public func _stdlib_bridgeNSErrorToErrorProtocol<
  T : _ObjectiveCBridgeableErrorProtocol
>(_ error: NSError, out: UnsafeMutablePointer<T>) -> Bool {
  if let bridged = T(_bridgedNSError: error) {
    out.initialize(with: bridged)
    return true
  } else {
    return false
  }
}

/// Helper protocol for _BridgedNSError, which used to provide
/// default implementations.
public protocol __BridgedNSError : ErrorProtocol {
  static var _nsErrorDomain: String { get }
}

// Allow two bridged NSError types to be compared.
public func ==<T: __BridgedNSError>(lhs: T, rhs: T) -> Bool
  where T: RawRepresentable, T.RawValue: SignedInteger {
  return lhs.rawValue.toIntMax() == rhs.rawValue.toIntMax()
}

public extension __BridgedNSError 
    where Self: RawRepresentable, Self.RawValue: SignedInteger {
  public final var _domain: String { return Self._nsErrorDomain }
  public final var _code: Int { return Int(rawValue.toIntMax()) }

  public init?(rawValue: RawValue) {
    self = unsafeBitCast(rawValue, to: Self.self)
  }

  public init?(_bridgedNSError: NSError) {
    if _bridgedNSError.domain != Self._nsErrorDomain {
      return nil
    }

    self.init(rawValue: RawValue(IntMax(_bridgedNSError.code)))
  }

  public final var hashValue: Int { return _code }
}

// Allow two bridged NSError types to be compared.
public func ==<T: __BridgedNSError>(lhs: T, rhs: T) -> Bool
  where T: RawRepresentable, T.RawValue: UnsignedInteger {
  return lhs.rawValue.toUIntMax() == rhs.rawValue.toUIntMax()
}


public extension __BridgedNSError
    where Self: RawRepresentable, Self.RawValue: UnsignedInteger {
  public final var _domain: String { return Self._nsErrorDomain }
  public final var _code: Int {
    return Int(bitPattern: UInt(rawValue.toUIntMax()))
  }

  public init?(rawValue: RawValue) {
    self = unsafeBitCast(rawValue, to: Self.self)
  }

  public init?(_bridgedNSError: NSError) {
    if _bridgedNSError.domain != Self._nsErrorDomain {
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
/// _ObjectiveCBridgeableErrorProtocol for such an enum.
public protocol _BridgedNSError : __BridgedNSError,
                                  RawRepresentable,
                                  _ObjectiveCBridgeableErrorProtocol,
                                  Hashable {
  /// The NSError domain to which this type is bridged.
  static var _nsErrorDomain: String { get }
}

/// Describes a bridged error that stores the underlying NSError, so
/// it can be queried.
public protocol _BridgedStoredNSError :
     __BridgedNSError, _ObjectiveCBridgeableErrorProtocol, CustomNSError {
  /// The type of an error code.
  associatedtype Code: _ErrorCodeProtocol

  /// The error code for the given error.
  var code: Code { get }

  //// Retrieves the embedded NSError.
  var _nsError: NSError { get }

  /// Create a new instance of the error type with the given embedded
  /// NSError.
  ///
  /// The \c error must have the appropriate domain for this error
  /// type.
  init(_nsError error: NSError)
}

/// Various helper implementations for _BridgedStoredNSError
public extension _BridgedStoredNSError
    where Code: RawRepresentable, Code.RawValue == Int {
  // FIXME: Generalize based on the Integer protocol once SE-0104 is
  // implemented.
  public var code: Code {
    return Code(rawValue: _nsError.code)!
  }

  /// Initialize an error within this domain with the given ``code``
  /// and ``userInfo``.
  public init(_ code: Code, userInfo: [String : AnyObject] = [:]) {
    self.init(_nsError: NSError(domain: Self._nsErrorDomain,
                                code: code.rawValue,
                                userInfo: userInfo))
  }
}

/// Implementation of __BridgedNSError for all _BridgedStoredNSErrors.
public extension _BridgedStoredNSError {
  /// Default implementation of ``init(_bridgedNSError)`` to provide
  /// bridging from NSError.
  public init?(_bridgedNSError error: NSError) {
    if error.domain != Self._nsErrorDomain {
      return nil
    }

    self.init(_nsError: error)
  }
}

/// Implementation of CustomNSError for all _BridgedStoredNSErrors.
public extension _BridgedStoredNSError {
  // FIXME: Would prefer to have a clear "extract an NSError
  // directly" operation.

  static var errorDomain: String { return _nsErrorDomain }

  var errorCode: Int { return _nsError.code }

  var errorUserInfo: [String : AnyObject] {
    var result: [String : AnyObject] = [:]
    for (key, value) in _nsError.userInfo {
      guard let stringKey = key as? String else { continue }
      result[stringKey] = value
    }
    return result;
  }
}

/// Implementation of Hashable for all _BridgedStoredNSErrors.
public extension _BridgedStoredNSError {
  var hashValue: Int {
    return _nsError.hashValue
  }
}

/// Describes the code of an error.
public protocol _ErrorCodeProtocol : Equatable {
  /// The corresponding error code.
  associatedtype ErrorType

  // FIXME: We want ErrorType to be _BridgedStoredNSError and have its
  // Code match Self, but we cannot express those requirements yet.
}

/// Allow one to match an error code against an arbitrary error.
public func ~= <Code: _ErrorCodeProtocol>(match: Code, error: ErrorProtocol)
    -> Bool
    where Code.ErrorType: _BridgedStoredNSError {
  guard let specificError = error as? Code.ErrorType else { return false }

  // FIXME: Work around IRGen crash when we set Code == Code.ErrorType.Code.
  let specificCode = specificError.code as! Code
  return match == specificCode
}

func == <T: _BridgedStoredNSError>(lhs: T, rhs: T) -> Bool {
  return lhs._nsError.isEqual(rhs._nsError)
}

/// Describes errors within the Cocoa error domain.
public struct NSCocoaError : _BridgedStoredNSError {
  public let _nsError: NSError

  public init(_nsError error: NSError) {
    precondition(error.domain == NSCocoaErrorDomain)
    self._nsError = error
  }

  public static var _nsErrorDomain: String { return NSCocoaErrorDomain }

  /// The error code itself.
  public struct Code : RawRepresentable, _ErrorCodeProtocol {
    public typealias ErrorType = NSCocoaError

    public let rawValue: Int

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }
  }
}

public extension NSCocoaError {
  private var _userInfo: [NSObject : AnyObject] {
    return (self as NSError).userInfo
  }

  /// The file path associated with the error, if any.
  var filePath: String? {
    return _userInfo[NSFilePathErrorKey] as? String
  }

  /// The string encoding associated with this error, if any.
  var stringEncoding: String.Encoding? {
    return (_userInfo[NSStringEncodingErrorKey] as? NSNumber)
             .map { String.Encoding(rawValue: $0.uintValue) }
  }

  /// The underlying error behind this error, if any.
  var underlying: ErrorProtocol? {
    return _userInfo[NSUnderlyingErrorKey] as? ErrorProtocol
  }

  /// The URL associated with this error, if any.
  var url: URL? {
    return _userInfo[NSURLErrorKey] as? URL
  }
}

extension NSCocoaError.Code {
  public static var fileNoSuchFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4)
  }
  public static var fileLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 255)
  }
  public static var fileReadUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 256)
  }
  public static var fileReadNoPermissionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 257)
  }
  public static var fileReadInvalidFileNameError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 258)
  }
  public static var fileReadCorruptFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 259)
  }
  public static var fileReadNoSuchFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 260)
  }
  public static var fileReadInapplicableStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 261)
  }
  public static var fileReadUnsupportedSchemeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 262)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var fileReadTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 263)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var fileReadUnknownStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 264)
  }

  public static var fileWriteUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 512)
  }
  public static var fileWriteNoPermissionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 513)
  }
  public static var fileWriteInvalidFileNameError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 514)
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 5.0)
  public static var fileWriteFileExistsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 516)
  }

  public static var fileWriteInapplicableStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 517)
  }
  public static var fileWriteUnsupportedSchemeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 518)
  }
  public static var fileWriteOutOfSpaceError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 640)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var fileWriteVolumeReadOnlyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 642)
  }

  @available(OSX, introduced: 10.11) @available(iOS, unavailable)
  public static var fileManagerUnmountUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 768)
  }

  @available(OSX, introduced: 10.11) @available(iOS, unavailable)
  public static var fileManagerUnmountBusyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 769)
  }

  public static var keyValueValidationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1024)
  }
  public static var formattingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 2048)
  }
  public static var userCancelledError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3072)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var featureUnsupportedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3328)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableNotLoadableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3584)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableArchitectureMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3585)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableRuntimeMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3586)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableLoadError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3587)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableLinkError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3588)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadCorruptError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3840)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadUnknownVersionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3841)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadStreamError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3842)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListWriteStreamError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3851)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var propertyListWriteInvalidError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3852)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionInterrupted: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4097)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionInvalid: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4099)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionReplyInvalid: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4101)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileUnavailableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4353)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileNotUploadedDueToQuotaError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4354)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileUbiquityServerNotAvailable: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4355)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityHandoffFailedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4608)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityConnectionUnavailableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4609)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityRemoteApplicationTimedOutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4610)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityHandoffUserInfoTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4611)
  }

  @available(OSX, introduced: 10.11) @available(iOS, introduced: 9.0)
  public static var coderReadCorruptError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4864)
  }

  @available(OSX, introduced: 10.11) @available(iOS, introduced: 9.0)
  public static var coderValueNotFoundError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4865)
  }
}

extension NSCocoaError {
  public static var fileNoSuchFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4)
  }
  public static var fileLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 255)
  }
  public static var fileReadUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 256)
  }
  public static var fileReadNoPermissionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 257)
  }
  public static var fileReadInvalidFileNameError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 258)
  }
  public static var fileReadCorruptFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 259)
  }
  public static var fileReadNoSuchFileError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 260)
  }
  public static var fileReadInapplicableStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 261)
  }
  public static var fileReadUnsupportedSchemeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 262)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var fileReadTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 263)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var fileReadUnknownStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 264)
  }

  public static var fileWriteUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 512)
  }
  public static var fileWriteNoPermissionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 513)
  }
  public static var fileWriteInvalidFileNameError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 514)
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 5.0)
  public static var fileWriteFileExistsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 516)
  }

  public static var fileWriteInapplicableStringEncodingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 517)
  }
  public static var fileWriteUnsupportedSchemeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 518)
  }
  public static var fileWriteOutOfSpaceError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 640)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var fileWriteVolumeReadOnlyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 642)
  }

  @available(OSX, introduced: 10.11) @available(iOS, unavailable)
  public static var fileManagerUnmountUnknownError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 768)
  }

  @available(OSX, introduced: 10.11) @available(iOS, unavailable)
  public static var fileManagerUnmountBusyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 769)
  }

  public static var keyValueValidationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1024)
  }
  public static var formattingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 2048)
  }
  public static var userCancelledError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3072)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var featureUnsupportedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3328)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableNotLoadableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3584)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableArchitectureMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3585)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableRuntimeMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3586)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableLoadError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3587)
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public static var executableLinkError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3588)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadCorruptError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3840)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadUnknownVersionError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3841)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListReadStreamError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3842)
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public static var propertyListWriteStreamError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3851)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var propertyListWriteInvalidError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 3852)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionInterrupted: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4097)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionInvalid: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4099)
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public static var xpcConnectionReplyInvalid: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4101)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileUnavailableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4353)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileNotUploadedDueToQuotaError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4354)
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public static var ubiquitousFileUbiquityServerNotAvailable: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4355)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityHandoffFailedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4608)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityConnectionUnavailableError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4609)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityRemoteApplicationTimedOutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4610)
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var userActivityHandoffUserInfoTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4611)
  }

  @available(OSX, introduced: 10.11) @available(iOS, introduced: 9.0)
  public static var coderReadCorruptError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4864)
  }

  @available(OSX, introduced: 10.11) @available(iOS, introduced: 9.0)
  public static var coderValueNotFoundError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 4865)
  }
}

extension NSCocoaError {
  @available(OSX, introduced: 10.11) @available(iOS, introduced: 9.0)
  public var isCoderError: Bool {
    return code.rawValue >= 4864 && code.rawValue <= 4991
  }

  @available(OSX, introduced: 10.5) @available(iOS, introduced: 2.0)
  public var isExecutableError: Bool {
    return code.rawValue >= 3584 && code.rawValue <= 3839
  }

  public var isFileError: Bool {
    return code.rawValue >= 0 && code.rawValue <= 1023
  }

  public var isFormattingError: Bool {
    return code.rawValue >= 2048 && code.rawValue <= 2559
  }

  @available(OSX, introduced: 10.6) @available(iOS, introduced: 4.0)
  public var isPropertyListError: Bool {
    return code.rawValue >= 3840 && code.rawValue <= 4095
  }

  @available(OSX, introduced: 10.9) @available(iOS, introduced: 7.0)
  public var isUbiquitousFileError: Bool {
    return code.rawValue >= 4352 && code.rawValue <= 4607
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public var isUserActivityError: Bool {
    return code.rawValue >= 4608 && code.rawValue <= 4863
  }

  public var isValidationError: Bool {
    return code.rawValue >= 1024 && code.rawValue <= 2047
  }

  @available(OSX, introduced: 10.8) @available(iOS, introduced: 6.0)
  public var isXPCConnectionError: Bool {
    return code.rawValue >= 4096 && code.rawValue <= 4224
  }
}

extension NSCocoaError.Code {
  @available(*, unavailable, renamed: "fileNoSuchFileError")
  public static var FileNoSuchFileError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileLockingError")
  public static var FileLockingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadUnknownError")
  public static var FileReadUnknownError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadNoPermissionError")
  public static var FileReadNoPermissionError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadInvalidFileNameError")
  public static var FileReadInvalidFileNameError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadCorruptFileError")
  public static var FileReadCorruptFileError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadNoSuchFileError")
  public static var FileReadNoSuchFileError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadInapplicableStringEncodingError")
  public static var FileReadInapplicableStringEncodingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadUnsupportedSchemeError")
  public static var FileReadUnsupportedSchemeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadTooLargeError")
  public static var FileReadTooLargeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileReadUnknownStringEncodingError")
  public static var FileReadUnknownStringEncodingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteUnknownError")
  public static var FileWriteUnknownError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteNoPermissionError")
  public static var FileWriteNoPermissionError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteInvalidFileNameError")
  public static var FileWriteInvalidFileNameError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteFileExistsError")
  public static var FileWriteFileExistsError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteInapplicableStringEncodingError")
  public static var FileWriteInapplicableStringEncodingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteUnsupportedSchemeError")
  public static var FileWriteUnsupportedSchemeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteOutOfSpaceError")
  public static var FileWriteOutOfSpaceError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileWriteVolumeReadOnlyError")
  public static var FileWriteVolumeReadOnlyError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileManagerUnmountUnknownError")
  public static var FileManagerUnmountUnknownError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileManagerUnmountBusyError")
  public static var FileManagerUnmountBusyError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "keyValueValidationError")
  public static var KeyValueValidationError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "formattingError")
  public static var FormattingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userCancelledError")
  public static var UserCancelledError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "featureUnsupportedError")
  public static var FeatureUnsupportedError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "executableNotLoadableError")
  public static var ExecutableNotLoadableError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "executableArchitectureMismatchError")
  public static var ExecutableArchitectureMismatchError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "executableRuntimeMismatchError")
  public static var ExecutableRuntimeMismatchError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "executableLoadError")
  public static var ExecutableLoadError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "executableLinkError")
  public static var ExecutableLinkError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "propertyListReadCorruptError")
  public static var PropertyListReadCorruptError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "propertyListReadUnknownVersionError")
  public static var PropertyListReadUnknownVersionError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "propertyListReadStreamError")
  public static var PropertyListReadStreamError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "propertyListWriteStreamError")
  public static var PropertyListWriteStreamError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "propertyListWriteInvalidError")
  public static var PropertyListWriteInvalidError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "xpcConnectionInterrupted")
  public static var XPCConnectionInterrupted: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "xpcConnectionInvalid")
  public static var XPCConnectionInvalid: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "xpcConnectionReplyInvalid")
  public static var XPCConnectionReplyInvalid: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "ubiquitousFileUnavailableError")
  public static var UbiquitousFileUnavailableError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "ubiquitousFileNotUploadedDueToQuotaError")
  public static var UbiquitousFileNotUploadedDueToQuotaError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "ubiquitousFileUbiquityServerNotAvailable")
  public static var UbiquitousFileUbiquityServerNotAvailable: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userActivityHandoffFailedError")
  public static var UserActivityHandoffFailedError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userActivityConnectionUnavailableError")
  public static var UserActivityConnectionUnavailableError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userActivityRemoteApplicationTimedOutError")
  public static var UserActivityRemoteApplicationTimedOutError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userActivityHandoffUserInfoTooLargeError")
  public static var UserActivityHandoffUserInfoTooLargeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "coderReadCorruptError")
  public static var CoderReadCorruptError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "coderValueNotFoundError")
  public static var CoderValueNotFoundError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
}

/// Describes errors in the URL error domain.
public struct NSURLError : _BridgedStoredNSError {
  public let _nsError: NSError

  public init(_nsError error: NSError) {
    precondition(error.domain == NSURLErrorDomain)
    self._nsError = error
  }

  public static var _nsErrorDomain: String { return NSURLErrorDomain }

  @objc public enum Code : Int, _ErrorCodeProtocol {
    public typealias ErrorType = NSURLError

    case unknown = -1
    case cancelled = -999
    case badURL = -1000
    case timedOut = -1001
    case unsupportedURL = -1002
    case cannotFindHost = -1003
    case cannotConnectToHost = -1004
    case networkConnectionLost = -1005
    case dnsLookupFailed = -1006
    case httpTooManyRedirects = -1007
    case resourceUnavailable = -1008
    case notConnectedToInternet = -1009
    case redirectToNonExistentLocation = -1010
    case badServerResponse = -1011
    case userCancelledAuthentication = -1012
    case userAuthenticationRequired = -1013
    case zeroByteResource = -1014
    case cannotDecodeRawData = -1015
    case cannotDecodeContentData = -1016
    case cannotParseResponse = -1017
    case fileDoesNotExist = -1100
    case fileIsDirectory = -1101
    case noPermissionsToReadFile = -1102
    case secureConnectionFailed = -1200
    case serverCertificateHasBadDate = -1201
    case serverCertificateUntrusted = -1202
    case serverCertificateHasUnknownRoot = -1203
    case serverCertificateNotYetValid = -1204
    case clientCertificateRejected = -1205
    case clientCertificateRequired = -1206
    case cannotLoadFromNetwork = -2000
    case cannotCreateFile = -3000
    case cannotOpenFile = -3001
    case cannotCloseFile = -3002
    case cannotWriteToFile = -3003
    case cannotRemoveFile = -3004
    case cannotMoveFile = -3005
    case downloadDecodingFailedMidStream = -3006
    case downloadDecodingFailedToComplete = -3007

    @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
    case internationalRoamingOff = -1018

    @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
    case callIsActive = -1019

    @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
    case dataNotAllowed = -1020

    @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
    case requestBodyStreamExhausted = -1021

    @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
    static var backgroundSessionRequiresSharedContainer: Code {
      return Code(rawValue: -995)!
    }

    @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
    static var backgroundSessionInUseByAnotherProcess: Code {
      return Code(rawValue: -996)!
    }

    @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
    static var backgroundSessionWasDisconnected: Code {
      return Code(rawValue: -997)!
    }
  }
}

public extension NSURLError {
  private var _userInfo: [NSObject : AnyObject] {
    return (self as NSError).userInfo
  }

  /// The URL which caused a load to fail.
  public var failingURL: URL? {
    return _userInfo[NSURLErrorFailingURLErrorKey] as? URL
  }

  /// The string for the URL which caused a load to fail. 
  public var failureURLString: String? {
    return _userInfo[NSURLErrorFailingURLStringErrorKey] as? String
  }

  /// The state of a failed SSL handshake.
  public var failureURLPeerTrust: SecTrust? {
    if let secTrust = _userInfo[NSURLErrorFailingURLPeerTrustErrorKey] {
      return (secTrust as! SecTrust)
    }

    return nil
  }
}

public extension NSURLError {
  public static var unknown: NSURLError.Code {
    return .unknown
  }

  public static var cancelled: NSURLError.Code {
    return .cancelled
  }

  public static var badURL: NSURLError.Code {
    return .badURL
  }

  public static var timedOut: NSURLError.Code {
    return .timedOut
  }

  public static var unsupportedURL: NSURLError.Code {
    return .unsupportedURL
  }

  public static var cannotFindHost: NSURLError.Code {
    return .cannotFindHost
  }

  public static var cannotConnectToHost: NSURLError.Code {
    return .cannotConnectToHost
  }

  public static var networkConnectionLost: NSURLError.Code {
    return .networkConnectionLost
  }

  public static var dnsLookupFailed: NSURLError.Code {
    return .dnsLookupFailed
  }

  public static var httpTooManyRedirects: NSURLError.Code {
    return .httpTooManyRedirects
  }

  public static var resourceUnavailable: NSURLError.Code {
    return .resourceUnavailable
  }

  public static var notConnectedToInternet: NSURLError.Code {
    return .notConnectedToInternet
  }

  public static var redirectToNonExistentLocation: NSURLError.Code {
    return .redirectToNonExistentLocation
  }

  public static var badServerResponse: NSURLError.Code {
    return .badServerResponse
  }

  public static var userCancelledAuthentication: NSURLError.Code {
    return .userCancelledAuthentication
  }

  public static var userAuthenticationRequired: NSURLError.Code {
    return .userAuthenticationRequired
  }

  public static var zeroByteResource: NSURLError.Code {
    return .zeroByteResource
  }

  public static var cannotDecodeRawData: NSURLError.Code {
    return .cannotDecodeRawData
  }

  public static var cannotDecodeContentData: NSURLError.Code {
    return .cannotDecodeContentData
  }

  public static var cannotParseResponse: NSURLError.Code {
    return .cannotParseResponse
  }

  public static var fileDoesNotExist: NSURLError.Code {
    return .fileDoesNotExist
  }

  public static var fileIsDirectory: NSURLError.Code {
    return .fileIsDirectory
  }

  public static var noPermissionsToReadFile: NSURLError.Code {
    return .noPermissionsToReadFile
  }

  public static var secureConnectionFailed: NSURLError.Code {
    return .secureConnectionFailed
  }

  public static var serverCertificateHasBadDate: NSURLError.Code {
    return .serverCertificateHasBadDate
  }

  public static var serverCertificateUntrusted: NSURLError.Code {
    return .serverCertificateUntrusted
  }

  public static var serverCertificateHasUnknownRoot: NSURLError.Code {
    return .serverCertificateHasUnknownRoot
  }

  public static var serverCertificateNotYetValid: NSURLError.Code {
    return .serverCertificateNotYetValid
  }

  public static var clientCertificateRejected: NSURLError.Code {
    return .clientCertificateRejected
  }

  public static var clientCertificateRequired: NSURLError.Code {
    return .clientCertificateRequired
  }

  public static var cannotLoadFromNetwork: NSURLError.Code {
    return .cannotLoadFromNetwork
  }

  public static var cannotCreateFile: NSURLError.Code {
    return .cannotCreateFile
  }

  public static var cannotOpenFile: NSURLError.Code {
    return .cannotOpenFile
  }

  public static var cannotCloseFile: NSURLError.Code {
    return .cannotCloseFile
  }

  public static var cannotWriteToFile: NSURLError.Code {
    return .cannotWriteToFile
  }

  public static var cannotRemoveFile: NSURLError.Code {
    return .cannotRemoveFile
  }

  public static var cannotMoveFile: NSURLError.Code {
    return .cannotMoveFile
  }

  public static var downloadDecodingFailedMidStream: NSURLError.Code {
    return .downloadDecodingFailedMidStream
  }

  public static var downloadDecodingFailedToComplete: NSURLError.Code {
    return .downloadDecodingFailedToComplete
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
  public static var internationalRoamingOff: NSURLError.Code {
    return .internationalRoamingOff
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
  public static var callIsActive: NSURLError.Code {
    return .callIsActive
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
  public static var dataNotAllowed: NSURLError.Code {
    return .dataNotAllowed
  }

  @available(OSX, introduced: 10.7) @available(iOS, introduced: 3.0)
  public static var requestBodyStreamExhausted: NSURLError.Code {
    return .requestBodyStreamExhausted
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var backgroundSessionRequiresSharedContainer: Code {
    return .backgroundSessionRequiresSharedContainer
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var backgroundSessionInUseByAnotherProcess: Code {
    return .backgroundSessionInUseByAnotherProcess
  }

  @available(OSX, introduced: 10.10) @available(iOS, introduced: 8.0)
  public static var backgroundSessionWasDisconnected: Code {
    return .backgroundSessionWasDisconnected
  }
}

extension NSURLError {
  @available(*, unavailable, renamed: "unknown")
  public static var Unknown: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cancelled")
  public static var Cancelled: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "badURL")
  public static var BadURL: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "timedOut")
  public static var TimedOut: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "unsupportedURL")
  public static var UnsupportedURL: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotFindHost")
  public static var CannotFindHost: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotConnectToHost")
  public static var CannotConnectToHost: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "networkConnectionLost")
  public static var NetworkConnectionLost: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "dnsLookupFailed")
  public static var DNSLookupFailed: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "httpTooManyRedirects")
  public static var HTTPTooManyRedirects: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "resourceUnavailable")
  public static var ResourceUnavailable: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "notConnectedToInternet")
  public static var NotConnectedToInternet: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "redirectToNonExistentLocation")
  public static var RedirectToNonExistentLocation: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "badServerResponse")
  public static var BadServerResponse: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userCancelledAuthentication")
  public static var UserCancelledAuthentication: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "userAuthenticationRequired")
  public static var UserAuthenticationRequired: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "zeroByteResource")
  public static var ZeroByteResource: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotDecodeRawData")
  public static var CannotDecodeRawData: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotDecodeContentData")
  public static var CannotDecodeContentData: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotParseResponse")
  public static var CannotParseResponse: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileDoesNotExist")
  public static var FileDoesNotExist: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "fileIsDirectory")
  public static var FileIsDirectory: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "noPermissionsToReadFile")
  public static var NoPermissionsToReadFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "secureConnectionFailed")
  public static var SecureConnectionFailed: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "serverCertificateHasBadDate")
  public static var ServerCertificateHasBadDate: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "serverCertificateUntrusted")
  public static var ServerCertificateUntrusted: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "serverCertificateHasUnknownRoot")
  public static var ServerCertificateHasUnknownRoot: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "serverCertificateNotYetValid")
  public static var ServerCertificateNotYetValid: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "clientCertificateRejected")
  public static var ClientCertificateRejected: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "clientCertificateRequired")
  public static var ClientCertificateRequired: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotLoadFromNetwork")
  public static var CannotLoadFromNetwork: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotCreateFile")
  public static var CannotCreateFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotOpenFile")
  public static var CannotOpenFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotCloseFile")
  public static var CannotCloseFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotWriteToFile")
  public static var CannotWriteToFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotRemoveFile")
  public static var CannotRemoveFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "cannotMoveFile")
  public static var CannotMoveFile: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "downloadDecodingFailedMidStream")
  public static var DownloadDecodingFailedMidStream: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "downloadDecodingFailedToComplete")
  public static var DownloadDecodingFailedToComplete: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "internationalRoamingOff")
  public static var InternationalRoamingOff: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "callIsActive")
  public static var CallIsActive: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "dataNotAllowed")
  public static var DataNotAllowed: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "requestBodyStreamExhausted")
  public static var RequestBodyStreamExhausted: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "backgroundSessionRequiresSharedContainer")
  public static var BackgroundSessionRequiresSharedContainer: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "backgroundSessionInUseByAnotherProcess")
  public static var BackgroundSessionInUseByAnotherProcess: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }

  @available(*, unavailable, renamed: "backgroundSessionWasDisconnected")
  public static var BackgroundSessionWasDisconnected: NSURLError.Code {
    fatalError("unavailable accessor can't be called")
  }
}

extension POSIXError : _BridgedNSError {
  public static var _nsErrorDomain: String { return NSPOSIXErrorDomain }
}

extension MachError : _BridgedNSError {
  public static var _nsErrorDomain: String { return NSMachErrorDomain }
}

public struct ErrorUserInfoKey : RawRepresentable, _SwiftNewtypeWrapper, Equatable, Hashable, _ObjectiveCBridgeable {
  public init(rawValue: String) { self.rawValue = rawValue }
  public var rawValue: String
}

public extension ErrorUserInfoKey {
  @available(*, deprecated, renamed: "NSUnderlyingErrorKey")
  static let underlyingErrorKey = ErrorUserInfoKey(rawValue: NSUnderlyingErrorKey)

  @available(*, deprecated, renamed: "NSLocalizedDescriptionKey")
  static let localizedDescriptionKey = ErrorUserInfoKey(rawValue: NSLocalizedDescriptionKey)

  @available(*, deprecated, renamed: "NSLocalizedFailureReasonErrorKey")
  static let localizedFailureReasonErrorKey = ErrorUserInfoKey(rawValue: NSLocalizedFailureReasonErrorKey)

  @available(*, deprecated, renamed: "NSLocalizedRecoverySuggestionErrorKey")
  static let localizedRecoverySuggestionErrorKey = ErrorUserInfoKey(rawValue: NSLocalizedRecoverySuggestionErrorKey)

  @available(*, deprecated, renamed: "NSLocalizedRecoveryOptionsErrorKey")
  static let localizedRecoveryOptionsErrorKey = ErrorUserInfoKey(rawValue: NSLocalizedRecoveryOptionsErrorKey)

  @available(*, deprecated, renamed: "NSRecoveryAttempterErrorKey")
  static let recoveryAttempterErrorKey = ErrorUserInfoKey(rawValue: NSRecoveryAttempterErrorKey)

  @available(*, deprecated, renamed: "NSHelpAnchorErrorKey")
  static let helpAnchorErrorKey = ErrorUserInfoKey(rawValue: NSHelpAnchorErrorKey)

  @available(*, deprecated, renamed: "NSStringEncodingErrorKey")
  static let stringEncodingErrorKey = ErrorUserInfoKey(rawValue: NSStringEncodingErrorKey)

  @available(*, deprecated, renamed: "NSURLErrorKey")
  static let NSURLErrorKey = ErrorUserInfoKey(rawValue: Foundation.NSURLErrorKey)

  @available(*, deprecated, renamed: "NSFilePathErrorKey")
  static let filePathErrorKey = ErrorUserInfoKey(rawValue: NSFilePathErrorKey)
}
