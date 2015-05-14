import Foundation
import CoreFoundation

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

/// An intrinsic used by the runtime to create an error when an
/// Objective-C API indicates failure but produces a nil error.
@asmname("_swift_allocNilObjCError")
func _allocNilObjCError() -> ErrorType {
  return _GenericObjCError.NilError
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
@asmname("swift_stdlib_bridgeNSErrorToErrorType")
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

public enum _NSCocoaError : Int, _ObjectiveCBridgeableErrorType {
  // Foundation errors
  case FileNoSuchFileError = 4
  case FileLockingError = 255
  case FileReadUnknownError = 256
  case FileReadNoPermissionError = 257
  case FileReadInvalidFileNameError = 258
  case FileReadCorruptFileError = 259
  case FileReadNoSuchFileError = 260
  case FileReadInapplicableStringEncodingError = 261
  case FileReadUnsupportedSchemeError = 262

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  case FileReadTooLargeError = 263

  case FileReadUnknownStringEncodingError = 264

  case FileWriteUnknownError = 512
  case FileWriteNoPermissionError = 513
  case FileWriteInvalidFileNameError = 514

  @available(OSX, introduced=10.7) @available(iOS, introduced=5.0)
  case FileWriteFileExistsError = 516

  case FileWriteInapplicableStringEncodingError = 517
  case FileWriteUnsupportedSchemeError = 518
  case FileWriteOutOfSpaceError = 640

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  case FileWriteVolumeReadOnlyError = 642

  case FileManagerUnmountUnknownError = 768

  case FileManagerUnmountBusyError = 769

  case KeyValueValidationError = 1024
  case FormattingError = 2048
  case UserCancelledError = 3072

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  case FeatureUnsupportedError = 3328

  case ExecutableNotLoadableError = 3584

  case ExecutableArchitectureMismatchError = 3585

  case ExecutableRuntimeMismatchError = 3586

  case ExecutableLoadError = 3587

  case ExecutableLinkError = 3588

  case PropertyListReadCorruptError = 3840

  case PropertyListReadUnknownVersionError = 3841

  case PropertyListReadStreamError = 3842

  case PropertyListWriteStreamError = 3851

  case PropertyListWriteInvalidError = 3852

  case XPCConnectionInterrupted = 4097

  case XPCConnectionInvalid = 4099

  case XPCConnectionReplyInvalid = 4101

  case UbiquitousFileUnavailableError = 4353

  case UbiquitousFileNotUploadedDueToQuotaError = 4354

  case UbiquitousFileUbiquityServerNotAvailable = 4355

  case UserActivityHandoffFailedError = 4608

  case UserActivityConnectionUnavailableError = 4609

  case UserActivityRemoteApplicationTimedOutError = 4610

  case UserActivityHandoffUserInfoTooLargeError = 4611

  case CoderReadCorruptError = 4864

  case CoderValueNotFoundError = 4865


  @available(OSX, introduced=10.11) @available(iOS, introduced=9.0)
  public var isCoderError: Bool {
    return rawValue >= 4864 && rawValue <= 4991;
  }

  @available(OSX, introduced=10.5) @available(iOS, introduced=2.0)
  public var isExecutableError: Bool {
    return rawValue >= 3584 && rawValue <= 3839;
  }

  public var isFileError: Bool {
    return rawValue >= 0 && rawValue <= 1023;
  }

  public var isFormattingError: Bool {
    return rawValue >= 2048 && rawValue <= 2559;
  }

  @available(OSX, introduced=10.6) @available(iOS, introduced=4.0)
  public var isPropertyListError: Bool {
    return rawValue >= 3840 && rawValue <= 4095;
  }

  @available(OSX, introduced=10.9) @available(iOS, introduced=7.0)
  public var isUbiquitousFileError: Bool {
    return rawValue >= 4352 && rawValue <= 4607;
  }

  @available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
  public var isUserActivityError: Bool {
    return rawValue >= 4608 && rawValue <= 4863;
  }

  public var isValidationError: Bool {
    return rawValue >= 1024 && rawValue <= 2047;
  }

  @available(OSX, introduced=10.8) @available(iOS, introduced=6.0)
  public var isXPCConnectionError: Bool {
    return rawValue >= 4096 && rawValue <= 4224;
  }

  // CoreData errors
  case ManagedObjectValidationError = 1550
  case ValidationMultipleErrorsError = 1560
  case ValidationMissingMandatoryPropertyError = 1570
  case ValidationRelationshipLacksMinimumCountError = 1580
  case ValidationRelationshipExceedsMaximumCountError = 1590
  case ValidationRelationshipDeniedDeleteError = 1600
  case ValidationNumberTooLargeError = 1610
  case ValidationNumberTooSmallError = 1620
  case ValidationDateTooLateError = 1630
  case ValidationDateTooSoonError = 1640
  case ValidationInvalidDateError = 1650
  case ValidationStringTooLongError = 1660
  case ValidationStringTooShortError = 1670
  case ValidationStringPatternMatchingError = 1680
  case ManagedObjectContextLockingError = 132000
  case PersistentStoreCoordinatorLockingError = 132010
  case ManagedObjectReferentialIntegrityError = 133000
  case ManagedObjectExternalRelationshipError = 133010
  case ManagedObjectMergeError = 133020
  case ManagedObjectConstraintMergeError = 133021
  case PersistentStoreInvalidTypeError = 134000
  case PersistentStoreTypeMismatchError = 134010
  case PersistentStoreIncompatibleSchemaError = 134020
  case PersistentStoreSaveError = 134030
  case PersistentStoreIncompleteSaveError = 134040
  case PersistentStoreSaveConflictsError = 134050
  case CoreDataError = 134060
  case PersistentStoreOperationError = 134070
  case PersistentStoreOpenError = 134080
  case PersistentStoreTimeoutError = 134090
  case PersistentStoreUnsupportedRequestTypeError = 134091
  case PersistentStoreIncompatibleVersionHashError = 134100
  case MigrationError = 134110
  case MigrationCancelledError = 134120
  case MigrationMissingSourceModelError = 134130
  case MigrationMissingMappingModelError = 134140
  case MigrationManagerSourceStoreError = 134150
  case MigrationManagerDestinationStoreError = 134160
  case EntityMigrationPolicyError = 134170
  case SQLiteError = 134180
  case InferredMappingModelError = 134190
  case ExternalRecordImportError = 134200

#if os(OSX)
  // AppKit errors
  case TextReadInapplicableDocumentTypeError = 65806
  case TextWriteInapplicableDocumentTypeError = 66062
  case ServiceApplicationNotFoundError = 66560
  case ServiceApplicationLaunchFailedError = 66561
  case ServiceRequestTimedOutError = 66562
  case ServiceInvalidPasteboardDataError = 66563
  case ServiceMalformedServiceDictionaryError = 66564
  case ServiceMiscellaneousError = 66800
  case SharingServiceNotConfiguredError = 67072

  public var isServiceError: Bool {
    return rawValue >= 66560 && rawValue <= 66817;
  }

  public var isSharingServiceError: Bool {
    return rawValue >= 67072 && rawValue <= 67327;
  }

  public var isTextReadWriteError: Bool {
    return rawValue >= 65792 && rawValue <= 66303;
  }
#endif

  public var _domain: String { return NSCocoaErrorDomain }
  public var _code: Int { return rawValue }

  public init?(_bridgedNSError: NSError) {
    if _bridgedNSError.domain != NSCocoaErrorDomain {
      return nil
    }

    if let result = _NSCocoaError(rawValue: _bridgedNSError.code) {
      self = result
    } else {
      return nil
    }
  }
}
