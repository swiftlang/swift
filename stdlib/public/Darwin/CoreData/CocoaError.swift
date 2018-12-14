//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import CoreData
import Foundation

// Code constants
extension CocoaError.Code {
  public static var managedObjectValidation: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrors: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryProperty: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCount: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCount: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDelete: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLarge: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmall: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLate: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoon: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDate: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLong: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShort: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatching: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLocking: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLocking: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrity: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationship: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMerge: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMerge: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidType: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatch: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchema: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSave: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompleteSave: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflicts: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  public static var coreData: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperation: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpen: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeout: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestType: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHash: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  public static var migration: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelled: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStore: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStore: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicy: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  public static var sqlite: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImport: CocoaError.Code {
    return CocoaError.Code(rawValue: 134200)
  }
}

// Code constants with names deprecated late in Swift 3
extension CocoaError.Code {
  @available(*, deprecated, renamed: "managedObjectValidation")
  public static var managedObjectValidationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  @available(*, deprecated, renamed: "validationMultipleErrors")
  public static var validationMultipleErrorsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  @available(*, deprecated, renamed: "validationMissingMandatoryProperty")
  public static var validationMissingMandatoryPropertyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  @available(*, deprecated, renamed: "validationRelationshipLacksMinimumCount")
  public static var validationRelationshipLacksMinimumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  @available(*, deprecated, renamed: "validationRelationshipExceedsMaximumCount")
  public static var validationRelationshipExceedsMaximumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  @available(*, deprecated, renamed: "validationRelationshipDeniedDelete")
  public static var validationRelationshipDeniedDeleteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  @available(*, deprecated, renamed: "validationNumberTooLarge")
  public static var validationNumberTooLargeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  @available(*, deprecated, renamed: "validationNumberTooSmall")
  public static var validationNumberTooSmallError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  @available(*, deprecated, renamed: "validationDateTooLate")
  public static var validationDateTooLateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  @available(*, deprecated, renamed: "validationDateTooSoon")
  public static var validationDateTooSoonError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  @available(*, deprecated, renamed: "validationInvalidDate")
  public static var validationInvalidDateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  @available(*, deprecated, renamed: "validationStringTooLong")
  public static var validationStringTooLongError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  @available(*, deprecated, renamed: "validationStringTooShort")
  public static var validationStringTooShortError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  @available(*, deprecated, renamed: "validationStringPatternMatching")
  public static var validationStringPatternMatchingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  @available(*, deprecated, renamed: "managedObjectContextLocking")
  public static var managedObjectContextLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  @available(*, deprecated, renamed: "persistentStoreCoordinatorLocking")
  public static var persistentStoreCoordinatorLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  @available(*, deprecated, renamed: "managedObjectReferentialIntegrity")
  public static var managedObjectReferentialIntegrityError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  @available(*, deprecated, renamed: "managedObjectExternalRelationship")
  public static var managedObjectExternalRelationshipError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  @available(*, deprecated, renamed: "managedObjectMerge")
  public static var managedObjectMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  @available(*, deprecated, renamed: "managedObjectConstraintMerge")
  public static var managedObjectConstraintMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  @available(*, deprecated, renamed: "persistentStoreInvalidType")
  public static var persistentStoreInvalidTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  @available(*, deprecated, renamed: "persistentStoreTypeMismatch")
  public static var persistentStoreTypeMismatchError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompatibleSchema")
  public static var persistentStoreIncompatibleSchemaError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  @available(*, deprecated, renamed: "persistentStoreSave")
  public static var persistentStoreSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompleteSave")
  public static var persistentStoreIncompleteSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  @available(*, deprecated, renamed: "persistentStoreSaveConflicts")
  public static var persistentStoreSaveConflictsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  @available(*, deprecated, renamed: "coreData")
  public static var coreDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  @available(*, deprecated, renamed: "persistentStoreOperation")
  public static var persistentStoreOperationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  @available(*, deprecated, renamed: "persistentStoreOpen")
  public static var persistentStoreOpenError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  @available(*, deprecated, renamed: "persistentStoreTimeout")
  public static var persistentStoreTimeoutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  @available(*, deprecated, renamed: "persistentStoreUnsupportedRequestType")
  public static var persistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompatibleVersionHash")
  public static var persistentStoreIncompatibleVersionHashError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  @available(*, deprecated, renamed: "migration")
  public static var migrationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  @available(*, deprecated, renamed: "migrationCancelled")
  public static var migrationCancelledError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  @available(*, deprecated, renamed: "migrationMissingSourceModel")
  public static var migrationMissingSourceModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  @available(*, deprecated, renamed: "migrationMissingMappingModel")
  public static var migrationMissingMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  @available(*, deprecated, renamed: "migrationManagerSourceStore")
  public static var migrationManagerSourceStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  @available(*, deprecated, renamed: "migrationManagerDestinationStore")
  public static var migrationManagerDestinationStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  @available(*, deprecated, renamed: "entityMigrationPolicy")
  public static var entityMigrationPolicyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  @available(*, deprecated, renamed: "sqlite")
  public static var sqliteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  @available(*, deprecated, renamed: "inferredMappingModel")
  public static var inferredMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  @available(*, deprecated, renamed: "externalRecordImport")
  public static var externalRecordImportError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134200)
  }
}

// Code constants
extension CocoaError {
  public static var managedObjectValidation: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  public static var validationMultipleErrors: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  public static var validationMissingMandatoryProperty: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  public static var validationRelationshipLacksMinimumCount: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  public static var validationRelationshipExceedsMaximumCount: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  public static var validationRelationshipDeniedDelete: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  public static var validationNumberTooLarge: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  public static var validationNumberTooSmall: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  public static var validationDateTooLate: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  public static var validationDateTooSoon: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  public static var validationInvalidDate: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  public static var validationStringTooLong: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  public static var validationStringTooShort: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  public static var validationStringPatternMatching: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  public static var managedObjectContextLocking: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  public static var persistentStoreCoordinatorLocking: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  public static var managedObjectReferentialIntegrity: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  public static var managedObjectExternalRelationship: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  public static var managedObjectMerge: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  public static var managedObjectConstraintMerge: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  public static var persistentStoreInvalidType: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  public static var persistentStoreTypeMismatch: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  public static var persistentStoreIncompatibleSchema: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  public static var persistentStoreSave: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  public static var persistentStoreIncompleteSave: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  public static var persistentStoreSaveConflicts: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  public static var coreData: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  public static var persistentStoreOperation: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  public static var persistentStoreOpen: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  public static var persistentStoreTimeout: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  public static var persistentStoreUnsupportedRequestType: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  public static var persistentStoreIncompatibleVersionHash: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  public static var migration: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  public static var migrationCancelled: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  public static var migrationMissingSourceModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  public static var migrationMissingMappingModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  public static var migrationManagerSourceStore: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  public static var migrationManagerDestinationStore: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  public static var entityMigrationPolicy: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  public static var sqlite: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  public static var inferredMappingModel: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  public static var externalRecordImport: CocoaError.Code {
    return CocoaError.Code(rawValue: 134200)
  }
}

// Code constants with names deprecated late in Swift 3
extension CocoaError {
  @available(*, deprecated, renamed: "managedObjectValidation")
  public static var managedObjectValidationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1550)
  }
  @available(*, deprecated, renamed: "validationMultipleErrors")
  public static var validationMultipleErrorsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1560)
  }
  @available(*, deprecated, renamed: "validationMissingMandatoryProperty")
  public static var validationMissingMandatoryPropertyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1570)
  }
  @available(*, deprecated, renamed: "validationRelationshipLacksMinimumCount")
  public static var validationRelationshipLacksMinimumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1580)
  }
  @available(*, deprecated, renamed: "validationRelationshipExceedsMaximumCount")
  public static var validationRelationshipExceedsMaximumCountError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1590)
  }
  @available(*, deprecated, renamed: "validationRelationshipDeniedDelete")
  public static var validationRelationshipDeniedDeleteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1600)
  }
  @available(*, deprecated, renamed: "validationNumberTooLarge")
  public static var validationNumberTooLargeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1610)
  }
  @available(*, deprecated, renamed: "validationNumberTooSmall")
  public static var validationNumberTooSmallError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1620)
  }
  @available(*, deprecated, renamed: "validationDateTooLate")
  public static var validationDateTooLateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1630)
  }
  @available(*, deprecated, renamed: "validationDateTooSoon")
  public static var validationDateTooSoonError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1640)
  }
  @available(*, deprecated, renamed: "validationInvalidDate")
  public static var validationInvalidDateError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1650)
  }
  @available(*, deprecated, renamed: "validationStringTooLong")
  public static var validationStringTooLongError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1660)
  }
  @available(*, deprecated, renamed: "validationStringTooShort")
  public static var validationStringTooShortError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1670)
  }
  @available(*, deprecated, renamed: "validationStringPatternMatching")
  public static var validationStringPatternMatchingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 1680)
  }
  @available(*, deprecated, renamed: "managedObjectContextLocking")
  public static var managedObjectContextLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132000)
  }
  @available(*, deprecated, renamed: "persistentStoreCoordinatorLocking")
  public static var persistentStoreCoordinatorLockingError: CocoaError.Code {
    return CocoaError.Code(rawValue: 132010)
  }
  @available(*, deprecated, renamed: "managedObjectReferentialIntegrity")
  public static var managedObjectReferentialIntegrityError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133000)
  }
  @available(*, deprecated, renamed: "managedObjectExternalRelationship")
  public static var managedObjectExternalRelationshipError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133010)
  }
  @available(*, deprecated, renamed: "managedObjectMerge")
  public static var managedObjectMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133020)
  }
  @available(*, deprecated, renamed: "managedObjectConstraintMerge")
  public static var managedObjectConstraintMergeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 133021)
  }
  @available(*, deprecated, renamed: "persistentStoreInvalidType")
  public static var persistentStoreInvalidTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134000)
  }
  @available(*, deprecated, renamed: "persistentStoreTypeMismatch")
  public static var persistentStoreTypeMismatchError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134010)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompatibleSchema")
  public static var persistentStoreIncompatibleSchemaError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134020)
  }
  @available(*, deprecated, renamed: "persistentStoreSave")
  public static var persistentStoreSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134030)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompleteSave")
  public static var persistentStoreIncompleteSaveError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134040)
  }
  @available(*, deprecated, renamed: "persistentStoreSaveConflicts")
  public static var persistentStoreSaveConflictsError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134050)
  }
  @available(*, deprecated, renamed: "coreData")
  public static var coreDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134060)
  }
  @available(*, deprecated, renamed: "persistentStoreOperation")
  public static var persistentStoreOperationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134070)
  }
  @available(*, deprecated, renamed: "persistentStoreOpen")
  public static var persistentStoreOpenError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134080)
  }
  @available(*, deprecated, renamed: "persistentStoreTimeout")
  public static var persistentStoreTimeoutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134090)
  }
  @available(*, deprecated, renamed: "persistentStoreUnsupportedRequestType")
  public static var persistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134091)
  }
  @available(*, deprecated, renamed: "persistentStoreIncompatibleVersionHash")
  public static var persistentStoreIncompatibleVersionHashError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134100)
  }
  @available(*, deprecated, renamed: "migration")
  public static var migrationError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134110)
  }
  @available(*, deprecated, renamed: "migrationCancelled")
  public static var migrationCancelledError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134120)
  }
  @available(*, deprecated, renamed: "migrationMissingSourceModel")
  public static var migrationMissingSourceModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134130)
  }
  @available(*, deprecated, renamed: "migrationMissingMappingModel")
  public static var migrationMissingMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134140)
  }
  @available(*, deprecated, renamed: "migrationManagerSourceStore")
  public static var migrationManagerSourceStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134150)
  }
  @available(*, deprecated, renamed: "migrationManagerDestinationStore")
  public static var migrationManagerDestinationStoreError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134160)
  }
  @available(*, deprecated, renamed: "entityMigrationPolicy")
  public static var entityMigrationPolicyError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134170)
  }
  @available(*, deprecated, renamed: "sqlite")
  public static var sqliteError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134180)
  }
  @available(*, deprecated, renamed: "inferredMappingModel")
  public static var inferredMappingModelError: CocoaError.Code {
    return CocoaError.Code(rawValue: 134190)
  }
  @available(*, deprecated, renamed: "externalRecordImport")
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

// Swift 2 names of code constants, removed from Swift 3
extension CocoaError {
  @available(*, unavailable, renamed: "managedObjectValidation")
  public static var ManagedObjectValidationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMultipleErrors")
  public static var ValidationMultipleErrorsError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationMissingMandatoryProperty")
  public static var ValidationMissingMandatoryPropertyError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipLacksMinimumCount")
  public static var ValidationRelationshipLacksMinimumCountError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipExceedsMaximumCount")
  public static var ValidationRelationshipExceedsMaximumCountError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationRelationshipDeniedDelete")
  public static var ValidationRelationshipDeniedDeleteError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooLarge")
  public static var ValidationNumberTooLargeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationNumberTooSmall")
  public static var ValidationNumberTooSmallError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooLate")
  public static var ValidationDateTooLateError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationDateTooSoon")
  public static var ValidationDateTooSoonError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationInvalidDate")
  public static var ValidationInvalidDateError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooLong")
  public static var ValidationStringTooLongError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringTooShort")
  public static var ValidationStringTooShortError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "validationStringPatternMatching")
  public static var ValidationStringPatternMatchingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectContextLocking")
  public static var ManagedObjectContextLockingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreCoordinatorLocking")
  public static var PersistentStoreCoordinatorLockingError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectReferentialIntegrity")
  public static var ManagedObjectReferentialIntegrityError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectExternalRelationship")
  public static var ManagedObjectExternalRelationshipError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectMerge")
  public static var ManagedObjectMergeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "managedObjectConstraintMerge")
  public static var ManagedObjectConstraintMergeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreInvalidType")
  public static var PersistentStoreInvalidTypeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTypeMismatch")
  public static var PersistentStoreTypeMismatchError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleSchema")
  public static var PersistentStoreIncompatibleSchemaError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSave")
  public static var PersistentStoreSaveError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompleteSave")
  public static var PersistentStoreIncompleteSaveError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreSaveConflicts")
  public static var PersistentStoreSaveConflictsError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "coreData")
  public static var CoreDataError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOperation")
  public static var PersistentStoreOperationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreOpen")
  public static var PersistentStoreOpenError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreTimeout")
  public static var PersistentStoreTimeoutError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreUnsupportedRequestType")
  public static var PersistentStoreUnsupportedRequestTypeError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "persistentStoreIncompatibleVersionHash")
  public static var PersistentStoreIncompatibleVersionHashError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migration")
  public static var MigrationError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationCancelled")
  public static var MigrationCancelledError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingSourceModel")
  public static var MigrationMissingSourceModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationMissingMappingModel")
  public static var MigrationMissingMappingModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerSourceStore")
  public static var MigrationManagerSourceStoreError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "migrationManagerDestinationStore")
  public static var MigrationManagerDestinationStoreError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "entityMigrationPolicy")
  public static var EntityMigrationPolicyError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "sqlite")
  public static var SQLiteError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "inferredMappingModel")
  public static var InferredMappingModelError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
  @available(*, unavailable, renamed: "externalRecordImport")
  public static var ExternalRecordImportError: CocoaError.Code {
    fatalError("unavailable accessor can't be called")
  }
}
