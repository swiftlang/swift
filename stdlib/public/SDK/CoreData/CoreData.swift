@_exported import CoreData
import Foundation

public extension NSCocoaError {
  @swift3_migration(renamed="managedObjectValidationError")
  public static var ManagedObjectValidationError: NSCocoaError {
    return NSCocoaError(rawValue: 1550)
  }
  @swift3_migration(renamed="validationMultipleErrorsError")
  public static var ValidationMultipleErrorsError: NSCocoaError {
    return NSCocoaError(rawValue: 1560)
  }
  @swift3_migration(renamed="validationMissingMandatoryPropertyError")
  public static var ValidationMissingMandatoryPropertyError: NSCocoaError {
    return NSCocoaError(rawValue: 1570)
  }
  @swift3_migration(renamed="validationRelationshipLacksMinimumCountError")
  public static var ValidationRelationshipLacksMinimumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1580)
  }
  @swift3_migration(renamed="validationRelationshipExceedsMaximumCountError")
  public static var ValidationRelationshipExceedsMaximumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1590)
  }
  @swift3_migration(renamed="validationRelationshipDeniedDevareError")
  public static var ValidationRelationshipDeniedDevareError: NSCocoaError {
    return NSCocoaError(rawValue: 1600)
  }
  @swift3_migration(renamed="validationNumberTooLargeError")
  public static var ValidationNumberTooLargeError: NSCocoaError {
    return NSCocoaError(rawValue: 1610)
  }
  @swift3_migration(renamed="validationNumberTooSmallError")
  public static var ValidationNumberTooSmallError: NSCocoaError {
    return NSCocoaError(rawValue: 1620)
  }
  @swift3_migration(renamed="validationDateTooLateError")
  public static var ValidationDateTooLateError: NSCocoaError {
    return NSCocoaError(rawValue: 1630)
  }
  @swift3_migration(renamed="validationDateTooSoonError")
  public static var ValidationDateTooSoonError: NSCocoaError {
    return NSCocoaError(rawValue: 1640)
  }
  @swift3_migration(renamed="validationInvalidDateError")
  public static var ValidationInvalidDateError: NSCocoaError {
    return NSCocoaError(rawValue: 1650)
  }
  @swift3_migration(renamed="validationStringTooLongError")
  public static var ValidationStringTooLongError: NSCocoaError {
    return NSCocoaError(rawValue: 1660)
  }
  @swift3_migration(renamed="validationStringTooShortError")
  public static var ValidationStringTooShortError: NSCocoaError {
    return NSCocoaError(rawValue: 1670)
  }
  @swift3_migration(renamed="validationStringPatternMatchingError")
  public static var ValidationStringPatternMatchingError: NSCocoaError {
    return NSCocoaError(rawValue: 1680)
  }
  @swift3_migration(renamed="managedObjectContextLockingError")
  public static var ManagedObjectContextLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132000)
  }
  @swift3_migration(renamed="persistentStoreCoordinatorLockingError")
  public static var PersistentStoreCoordinatorLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132010)
  }
  @swift3_migration(renamed="managedObjectReferentialIntegrityError")
  public static var ManagedObjectReferentialIntegrityError: NSCocoaError {
    return NSCocoaError(rawValue: 133000)
  }
  @swift3_migration(renamed="managedObjectExternalRelationshipError")
  public static var ManagedObjectExternalRelationshipError: NSCocoaError {
    return NSCocoaError(rawValue: 133010)
  }
  @swift3_migration(renamed="managedObjectMergeError")
  public static var ManagedObjectMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133020)
  }
  @swift3_migration(renamed="managedObjectConstraintMergeError")
  public static var ManagedObjectConstraintMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133021)
  }
  @swift3_migration(renamed="persistentStoreInvalidTypeError")
  public static var PersistentStoreInvalidTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134000)
  }
  @swift3_migration(renamed="persistentStoreTypeMismatchError")
  public static var PersistentStoreTypeMismatchError: NSCocoaError {
    return NSCocoaError(rawValue: 134010)
  }
  @swift3_migration(renamed="persistentStoreIncompatibleSchemaError")
  public static var PersistentStoreIncompatibleSchemaError: NSCocoaError {
    return NSCocoaError(rawValue: 134020)
  }
  @swift3_migration(renamed="persistentStoreSaveError")
  public static var PersistentStoreSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134030)
  }
  @swift3_migration(renamed="persistentStoreIncompvareSaveError")
  public static var PersistentStoreIncompvareSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134040)
  }
  @swift3_migration(renamed="persistentStoreSaveConflictsError")
  public static var PersistentStoreSaveConflictsError: NSCocoaError {
    return NSCocoaError(rawValue: 134050)
  }
  @swift3_migration(renamed="coreDataError")
  public static var CoreDataError: NSCocoaError {
    return NSCocoaError(rawValue: 134060)
  }
  @swift3_migration(renamed="persistentStoreOperationError")
  public static var PersistentStoreOperationError: NSCocoaError {
    return NSCocoaError(rawValue: 134070)
  }
  @swift3_migration(renamed="persistentStoreOpenError")
  public static var PersistentStoreOpenError: NSCocoaError {
    return NSCocoaError(rawValue: 134080)
  }
  @swift3_migration(renamed="persistentStoreTimeoutError")
  public static var PersistentStoreTimeoutError: NSCocoaError {
    return NSCocoaError(rawValue: 134090)
  }
  @swift3_migration(renamed="persistentStoreUnsupportedRequestTypeError")
  public static var PersistentStoreUnsupportedRequestTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134091)
  }
  @swift3_migration(renamed="persistentStoreIncompatibleVersionHashError")
  public static var PersistentStoreIncompatibleVersionHashError: NSCocoaError {
    return NSCocoaError(rawValue: 134100)
  }
  @swift3_migration(renamed="migrationError")
  public static var MigrationError: NSCocoaError {
    return NSCocoaError(rawValue: 134110)
  }
  @swift3_migration(renamed="migrationCancelledError")
  public static var MigrationCancelledError: NSCocoaError {
    return NSCocoaError(rawValue: 134120)
  }
  @swift3_migration(renamed="migrationMissingSourceModelError")
  public static var MigrationMissingSourceModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134130)
  }
  @swift3_migration(renamed="migrationMissingMappingModelError")
  public static var MigrationMissingMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134140)
  }
  @swift3_migration(renamed="migrationManagerSourceStoreError")
  public static var MigrationManagerSourceStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134150)
  }
  @swift3_migration(renamed="migrationManagerDestinationStoreError")
  public static var MigrationManagerDestinationStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134160)
  }
  @swift3_migration(renamed="entityMigrationPolicyError")
  public static var EntityMigrationPolicyError: NSCocoaError {
    return NSCocoaError(rawValue: 134170)
  }
  @swift3_migration(renamed="sqliteError")
  public static var SQLiteError: NSCocoaError {
    return NSCocoaError(rawValue: 134180)
  }
  @swift3_migration(renamed="inferredMappingModelError")
  public static var InferredMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134190)
  }
  @swift3_migration(renamed="externalRecordImportError")
  public static var ExternalRecordImportError: NSCocoaError {
    return NSCocoaError(rawValue: 134200)
  }
}

