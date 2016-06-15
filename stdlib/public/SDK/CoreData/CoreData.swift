@_exported import CoreData
import Foundation

extension NSCocoaError {
  public static var managedObjectValidationError: NSCocoaError {
    return NSCocoaError(rawValue: 1550)
  }
  public static var validationMultipleErrorsError: NSCocoaError {
    return NSCocoaError(rawValue: 1560)
  }
  public static var validationMissingMandatoryPropertyError: NSCocoaError {
    return NSCocoaError(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCountError: NSCocoaError {
    return NSCocoaError(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDevareError: NSCocoaError {
    return NSCocoaError(rawValue: 1600)
  }
  public static var validationNumberTooLargeError: NSCocoaError {
    return NSCocoaError(rawValue: 1610)
  }
  public static var validationNumberTooSmallError: NSCocoaError {
    return NSCocoaError(rawValue: 1620)
  }
  public static var validationDateTooLateError: NSCocoaError {
    return NSCocoaError(rawValue: 1630)
  }
  public static var validationDateTooSoonError: NSCocoaError {
    return NSCocoaError(rawValue: 1640)
  }
  public static var validationInvalidDateError: NSCocoaError {
    return NSCocoaError(rawValue: 1650)
  }
  public static var validationStringTooLongError: NSCocoaError {
    return NSCocoaError(rawValue: 1660)
  }
  public static var validationStringTooShortError: NSCocoaError {
    return NSCocoaError(rawValue: 1670)
  }
  public static var validationStringPatternMatchingError: NSCocoaError {
    return NSCocoaError(rawValue: 1680)
  }
  public static var managedObjectContextLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLockingError: NSCocoaError {
    return NSCocoaError(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrityError: NSCocoaError {
    return NSCocoaError(rawValue: 133000)
  }
  public static var managedObjectExternalRelationshipError: NSCocoaError {
    return NSCocoaError(rawValue: 133010)
  }
  public static var managedObjectMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133020)
  }
  public static var managedObjectConstraintMergeError: NSCocoaError {
    return NSCocoaError(rawValue: 133021)
  }
  public static var persistentStoreInvalidTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatchError: NSCocoaError {
    return NSCocoaError(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchemaError: NSCocoaError {
    return NSCocoaError(rawValue: 134020)
  }
  public static var persistentStoreSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134030)
  }
  public static var persistentStoreIncompvareSaveError: NSCocoaError {
    return NSCocoaError(rawValue: 134040)
  }
  public static var persistentStoreSaveConflictsError: NSCocoaError {
    return NSCocoaError(rawValue: 134050)
  }
  public static var coreDataError: NSCocoaError {
    return NSCocoaError(rawValue: 134060)
  }
  public static var persistentStoreOperationError: NSCocoaError {
    return NSCocoaError(rawValue: 134070)
  }
  public static var persistentStoreOpenError: NSCocoaError {
    return NSCocoaError(rawValue: 134080)
  }
  public static var persistentStoreTimeoutError: NSCocoaError {
    return NSCocoaError(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHashError: NSCocoaError {
    return NSCocoaError(rawValue: 134100)
  }
  public static var migrationError: NSCocoaError {
    return NSCocoaError(rawValue: 134110)
  }
  public static var migrationCancelledError: NSCocoaError {
    return NSCocoaError(rawValue: 134120)
  }
  public static var migrationMissingSourceModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134130)
  }
  public static var migrationMissingMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134140)
  }
  public static var migrationManagerSourceStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134150)
  }
  public static var migrationManagerDestinationStoreError: NSCocoaError {
    return NSCocoaError(rawValue: 134160)
  }
  public static var entityMigrationPolicyError: NSCocoaError {
    return NSCocoaError(rawValue: 134170)
  }
  public static var sqliteError: NSCocoaError {
    return NSCocoaError(rawValue: 134180)
  }
  public static var inferredMappingModelError: NSCocoaError {
    return NSCocoaError(rawValue: 134190)
  }
  public static var externalRecordImportError: NSCocoaError {
    return NSCocoaError(rawValue: 134200)
  }
}

extension NSCocoaError {
  @available(*, unavailable, renamed: "managedObjectValidationError")
  public static var ManagedObjectValidationError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMultipleErrorsError")
  public static var ValidationMultipleErrorsError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMissingMandatoryPropertyError")
  public static var ValidationMissingMandatoryPropertyError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipLacksMinimumCountError")
  public static var ValidationRelationshipLacksMinimumCountError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipExceedsMaximumCountError")
  public static var ValidationRelationshipExceedsMaximumCountError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipDeniedDevareError")
  public static var ValidationRelationshipDeniedDevareError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooLargeError")
  public static var ValidationNumberTooLargeError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooSmallError")
  public static var ValidationNumberTooSmallError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooLateError")
  public static var ValidationDateTooLateError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooSoonError")
  public static var ValidationDateTooSoonError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationInvalidDateError")
  public static var ValidationInvalidDateError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooLongError")
  public static var ValidationStringTooLongError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooShortError")
  public static var ValidationStringTooShortError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringPatternMatchingError")
  public static var ValidationStringPatternMatchingError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectContextLockingError")
  public static var ManagedObjectContextLockingError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreCoordinatorLockingError")
  public static var PersistentStoreCoordinatorLockingError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectReferentialIntegrityError")
  public static var ManagedObjectReferentialIntegrityError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectExternalRelationshipError")
  public static var ManagedObjectExternalRelationshipError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectMergeError")
  public static var ManagedObjectMergeError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectConstraintMergeError")
  public static var ManagedObjectConstraintMergeError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreInvalidTypeError")
  public static var PersistentStoreInvalidTypeError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTypeMismatchError")
  public static var PersistentStoreTypeMismatchError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleSchemaError")
  public static var PersistentStoreIncompatibleSchemaError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveError")
  public static var PersistentStoreSaveError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompvareSaveError")
  public static var PersistentStoreIncompvareSaveError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveConflictsError")
  public static var PersistentStoreSaveConflictsError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "coreDataError")
  public static var CoreDataError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOperationError")
  public static var PersistentStoreOperationError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOpenError")
  public static var PersistentStoreOpenError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTimeoutError")
  public static var PersistentStoreTimeoutError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreUnsupportedRequestTypeError")
  public static var PersistentStoreUnsupportedRequestTypeError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleVersionHashError")
  public static var PersistentStoreIncompatibleVersionHashError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationError")
  public static var MigrationError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationCancelledError")
  public static var MigrationCancelledError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingSourceModelError")
  public static var MigrationMissingSourceModelError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingMappingModelError")
  public static var MigrationMissingMappingModelError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerSourceStoreError")
  public static var MigrationManagerSourceStoreError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerDestinationStoreError")
  public static var MigrationManagerDestinationStoreError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "entityMigrationPolicyError")
  public static var EntityMigrationPolicyError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "sqliteError")
  public static var SQLiteError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "inferredMappingModelError")
  public static var InferredMappingModelError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "externalRecordImportError")
  public static var ExternalRecordImportError: NSCocoaError {
    fatalError("unavailable accessor can't be called")
  }
}

extension NSManagedObjectContext {
  public func fetch<T: NSFetchRequestResult>(_ request: NSFetchRequest<T>) throws -> [T] {
    return try fetch(unsafeBitCast(request, to: NSFetchRequest<NSFetchRequestResult>.self)) as! [T]
  }

  public func count<T: NSFetchRequestResult>(for request: NSFetchRequest<T>) throws -> Int {
    return try count(for: unsafeBitCast(request, to: NSFetchRequest<NSFetchRequestResult>.self))
  }
}
