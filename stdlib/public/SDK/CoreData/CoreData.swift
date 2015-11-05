@_exported import CoreData
import Foundation

public extension NSCocoaError {
  public static var ManagedObjectValidationError: NSCocoaError {
    return NSCocoaError(rawValue: 1550)
  }
  public static var ValidationMultipleErrorsError: NSCocoaError {
    return NSCocoaError(rawValue: 1560)
  }
  public static var ValidationMissingMandatoryPropertyError: NSCocoaError {
    return NSCocoaError(rawValue: 1570)
  }
  public static var ValidationRelationshipLacksMinimumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1580)
  }
  public static var ValidationRelationshipExceedsMaximumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1590)
  }
  public static var ValidationRelationshipDeniedDevareError: NSCocoaError {
    return NSCocoaError(rawValue: 1600)
  }
  public static var ValidationNumberTooLargeError: NSCocoaError {
    return NSCocoaError(rawValue: 1610)
  }
  public static var ValidationNumberTooSmallError: NSCocoaError {
    return NSCocoaError(rawValue: 1620)
  }
  public static var ValidationDateTooLateError: NSCocoaError {
    return NSCocoaError(rawValue: 1630)
  }
  public static var ValidationDateTooSoonError: NSCocoaError {
    return NSCocoaError(rawValue: 1640)
  }
  public static var ValidationInvalidDateError: NSCocoaError {
    return NSCocoaError(rawValue: 1650)
  }
  public static var ValidationStringTooLongError: NSCocoaError {
    return NSCocoaError(rawValue: 1660)
  }
  public static var ValidationStringTooShortError: NSCocoaError {
    return NSCocoaError(rawValue: 1670)
  }
  public static var ValidationStringPatternMatchingError: NSCocoaError {
    return NSCocoaError(rawValue: 1680)
  }
  public static var ManagedObjectContextLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132000)
  }
  public static var PersistentStoreCoordinatorLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132010)
  }
  public static var ManagedObjectReferentialIntegrityError: NSCocoaError {
    return NSCocoaError(rawValue: 133000)
  }
  public static var ManagedObjectExternalRelationshipError: NSCocoaError {
    return NSCocoaError(rawValue: 133010)
  }
  public static var ManagedObjectMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133020)
  }
  public static var ManagedObjectConstraintMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133021)
  }
  public static var PersistentStoreInvalidTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134000)
  }
  public static var PersistentStoreTypeMismatchError: NSCocoaError {
    return NSCocoaError(rawValue: 134010)
  }
  public static var PersistentStoreIncompatibleSchemaError: NSCocoaError {
    return NSCocoaError(rawValue: 134020)
  }
  public static var PersistentStoreSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134030)
  }
  public static var PersistentStoreIncompvareSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134040)
  }
  public static var PersistentStoreSaveConflictsError: NSCocoaError {
    return NSCocoaError(rawValue: 134050)
  }
  public static var CoreDataError: NSCocoaError {
    return NSCocoaError(rawValue: 134060)
  }
  public static var PersistentStoreOperationError: NSCocoaError {
    return NSCocoaError(rawValue: 134070)
  }
  public static var PersistentStoreOpenError: NSCocoaError {
    return NSCocoaError(rawValue: 134080)
  }
  public static var PersistentStoreTimeoutError: NSCocoaError {
    return NSCocoaError(rawValue: 134090)
  }
  public static var PersistentStoreUnsupportedRequestTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134091)
  }
  public static var PersistentStoreIncompatibleVersionHashError: NSCocoaError {
    return NSCocoaError(rawValue: 134100)
  }
  public static var MigrationError: NSCocoaError {
    return NSCocoaError(rawValue: 134110)
  }
  public static var MigrationCancelledError: NSCocoaError {
    return NSCocoaError(rawValue: 134120)
  }
  public static var MigrationMissingSourceModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134130)
  }
  public static var MigrationMissingMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134140)
  }
  public static var MigrationManagerSourceStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134150)
  }
  public static var MigrationManagerDestinationStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134160)
  }
  public static var EntityMigrationPolicyError: NSCocoaError {
    return NSCocoaError(rawValue: 134170)
  }
  public static var SQLiteError: NSCocoaError {
    return NSCocoaError(rawValue: 134180)
  }
  public static var InferredMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134190)
  }
  public static var ExternalRecordImportError: NSCocoaError {
    return NSCocoaError(rawValue: 134200)
  }
}

