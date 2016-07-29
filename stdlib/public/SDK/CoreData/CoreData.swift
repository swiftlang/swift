@_exported import CoreData
import Foundation

extension CocoaError.Code {
  public static var managedObjectValidationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrorsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryPropertyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDevareError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLargeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmallError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoonError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLongError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShortError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatchingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrityError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationshipError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatchError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchemaError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompvareSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflictsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  public static var coreDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpenError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeoutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHashError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  public static var migrationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelledError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  public static var sqliteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImportError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134200)
  }
}

extension CocoaError {
  public static var managedObjectValidationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrorsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryPropertyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDevareError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLargeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmallError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoonError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLongError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShortError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatchingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrityError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationshipError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatchError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchemaError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompvareSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflictsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  public static var coreDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpenError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeoutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHashError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  public static var migrationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelledError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  public static var sqliteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImportError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134200)
  }
}

extension CocoaError {
  /// Object that failed to validate for a validation error.
  public var validationObject: Any? {
    return userInfo[NSValidationObjectErrorKey]
  }

  /// Key that failed to validate for a validation error.
  public var validationKey: String? {
    return userInfo[NSValidationKeyErrorKey] as? String
  }

  /// For predicate-based validation, the predicate for the condition
  /// that failed to validate.
  public var validationPredicate: NSPredicate? {
    return userInfo[NSValidationPredicateErrorKey] as? NSPredicate
  }

  /// The value for the key that failed to validate for a validation
  /// error.
  public var validationValue: Any? {
    return userInfo[NSValidationValueErrorKey]
  }

  /// Stores prompting an error.
  public var affectedStores: [AnyObject]? {
    return userInfo[NSAffectedStoresErrorKey] as? [AnyObject]
  }

  /// Objects prompting an error.
  public var affectedObjects: [AnyObject]? {
    return userInfo[NSAffectedObjectsErrorKey] as? [AnyObject]
  }

  /// The conflicts that arise from a persistent store.
  public var persistentStoreSaveConflicts: [NSMergeConflict]? {
    return userInfo[NSPersistentStoreSaveConflictsErrorKey] as? [NSMergeConflict]
  }
}

extension CocoaError {
  @available(*, unavailable, renamed: "managedObjectValidationError")
  public static var ManagedObjectValidationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMultipleErrorsError")
  public static var ValidationMultipleErrorsError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMissingMandatoryPropertyError")
  public static var ValidationMissingMandatoryPropertyError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipLacksMinimumCountError")
  public static var ValidationRelationshipLacksMinimumCountError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipExceedsMaximumCountError")
  public static var ValidationRelationshipExceedsMaximumCountError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipDeniedDevareError")
  public static var ValidationRelationshipDeniedDevareError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooLargeError")
  public static var ValidationNumberTooLargeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooSmallError")
  public static var ValidationNumberTooSmallError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooLateError")
  public static var ValidationDateTooLateError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooSoonError")
  public static var ValidationDateTooSoonError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationInvalidDateError")
  public static var ValidationInvalidDateError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooLongError")
  public static var ValidationStringTooLongError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooShortError")
  public static var ValidationStringTooShortError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringPatternMatchingError")
  public static var ValidationStringPatternMatchingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectContextLockingError")
  public static var ManagedObjectContextLockingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreCoordinatorLockingError")
  public static var PersistentStoreCoordinatorLockingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectReferentialIntegrityError")
  public static var ManagedObjectReferentialIntegrityError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectExternalRelationshipError")
  public static var ManagedObjectExternalRelationshipError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectMergeError")
  public static var ManagedObjectMergeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectConstraintMergeError")
  public static var ManagedObjectConstraintMergeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreInvalidTypeError")
  public static var PersistentStoreInvalidTypeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTypeMismatchError")
  public static var PersistentStoreTypeMismatchError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleSchemaError")
  public static var PersistentStoreIncompatibleSchemaError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveError")
  public static var PersistentStoreSaveError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompvareSaveError")
  public static var PersistentStoreIncompvareSaveError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveConflictsError")
  public static var PersistentStoreSaveConflictsError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "coreDataError")
  public static var CoreDataError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOperationError")
  public static var PersistentStoreOperationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOpenError")
  public static var PersistentStoreOpenError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTimeoutError")
  public static var PersistentStoreTimeoutError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreUnsupportedRequestTypeError")
  public static var PersistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleVersionHashError")
  public static var PersistentStoreIncompatibleVersionHashError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationError")
  public static var MigrationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationCancelledError")
  public static var MigrationCancelledError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingSourceModelError")
  public static var MigrationMissingSourceModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingMappingModelError")
  public static var MigrationMissingMappingModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerSourceStoreError")
  public static var MigrationManagerSourceStoreError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerDestinationStoreError")
  public static var MigrationManagerDestinationStoreError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "entityMigrationPolicyError")
  public static var EntityMigrationPolicyError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "sqliteError")
  public static var SQLiteError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "inferredMappingModelError")
  public static var InferredMappingModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "externalRecordImportError")
  public static var ExternalRecordImportError: CocoaError.Code {
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
