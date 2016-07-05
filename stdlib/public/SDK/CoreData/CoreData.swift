@_exported import CoreData
import Foundation

extension NSCocoaError.Code {
  public static var managedObjectValidationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrorsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryPropertyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCountError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCountError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDevareError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmallError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLateError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoonError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDateError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLongError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShortError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatchingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrityError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationshipError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMergeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMergeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchemaError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSaveError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompvareSaveError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflictsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134050)
  }
  public static var coreDataError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpenError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeoutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHashError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134100)
  }
  public static var migrationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelledError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStoreError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStoreError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134170)
  }
  public static var sqliteError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImportError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134200)
  }
}

extension NSCocoaError {
  public static var managedObjectValidationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrorsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryPropertyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCountError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCountError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDevareError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLargeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmallError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLateError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoonError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDateError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLongError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShortError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatchingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLockingError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrityError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationshipError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMergeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMergeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatchError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchemaError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSaveError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompvareSaveError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflictsError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134050)
  }
  public static var coreDataError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpenError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeoutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHashError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134100)
  }
  public static var migrationError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelledError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStoreError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStoreError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicyError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134170)
  }
  public static var sqliteError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModelError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImportError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 134200)
  }
}

extension NSCocoaError {
  @available(*, unavailable, renamed: "managedObjectValidationError")
  public static var ManagedObjectValidationError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMultipleErrorsError")
  public static var ValidationMultipleErrorsError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMissingMandatoryPropertyError")
  public static var ValidationMissingMandatoryPropertyError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipLacksMinimumCountError")
  public static var ValidationRelationshipLacksMinimumCountError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipExceedsMaximumCountError")
  public static var ValidationRelationshipExceedsMaximumCountError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipDeniedDevareError")
  public static var ValidationRelationshipDeniedDevareError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooLargeError")
  public static var ValidationNumberTooLargeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooSmallError")
  public static var ValidationNumberTooSmallError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooLateError")
  public static var ValidationDateTooLateError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooSoonError")
  public static var ValidationDateTooSoonError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationInvalidDateError")
  public static var ValidationInvalidDateError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooLongError")
  public static var ValidationStringTooLongError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooShortError")
  public static var ValidationStringTooShortError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringPatternMatchingError")
  public static var ValidationStringPatternMatchingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectContextLockingError")
  public static var ManagedObjectContextLockingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreCoordinatorLockingError")
  public static var PersistentStoreCoordinatorLockingError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectReferentialIntegrityError")
  public static var ManagedObjectReferentialIntegrityError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectExternalRelationshipError")
  public static var ManagedObjectExternalRelationshipError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectMergeError")
  public static var ManagedObjectMergeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectConstraintMergeError")
  public static var ManagedObjectConstraintMergeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreInvalidTypeError")
  public static var PersistentStoreInvalidTypeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTypeMismatchError")
  public static var PersistentStoreTypeMismatchError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleSchemaError")
  public static var PersistentStoreIncompatibleSchemaError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveError")
  public static var PersistentStoreSaveError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompvareSaveError")
  public static var PersistentStoreIncompvareSaveError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveConflictsError")
  public static var PersistentStoreSaveConflictsError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "coreDataError")
  public static var CoreDataError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOperationError")
  public static var PersistentStoreOperationError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOpenError")
  public static var PersistentStoreOpenError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTimeoutError")
  public static var PersistentStoreTimeoutError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreUnsupportedRequestTypeError")
  public static var PersistentStoreUnsupportedRequestTypeError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleVersionHashError")
  public static var PersistentStoreIncompatibleVersionHashError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationError")
  public static var MigrationError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationCancelledError")
  public static var MigrationCancelledError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingSourceModelError")
  public static var MigrationMissingSourceModelError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingMappingModelError")
  public static var MigrationMissingMappingModelError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerSourceStoreError")
  public static var MigrationManagerSourceStoreError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerDestinationStoreError")
  public static var MigrationManagerDestinationStoreError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "entityMigrationPolicyError")
  public static var EntityMigrationPolicyError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "sqliteError")
  public static var SQLiteError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "inferredMappingModelError")
  public static var InferredMappingModelError: NSCocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "externalRecordImportError")
  public static var ExternalRecordImportError: NSCocoaError.Code {
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
