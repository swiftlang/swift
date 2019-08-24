//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import CloudKit // Clang module

@nonobjc
@available(macOS 10.14, iOS 12.0, tvOS 12.0, watchOS 5.0, *)
extension CKFetchRecordZoneChangesOperation {
    @available(swift 4.2)
    public convenience init(recordZoneIDs: [CKRecordZone.ID]? = nil, configurationsByRecordZoneID: [CKRecordZone.ID: ZoneConfiguration]? = nil) {
        self.init()
        self.recordZoneIDs = recordZoneIDs
        self.configurationsByRecordZoneID = configurationsByRecordZoneID
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKModifyRecordZonesOperation {
    @available(swift 4.2)
    public convenience init(recordZonesToSave: [CKRecordZone]? = nil, recordZoneIDsToDelete: [CKRecordZone.ID]? = nil) {
        self.init()
        self.recordZonesToSave = recordZonesToSave
        self.recordZoneIDsToDelete = recordZoneIDsToDelete
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKModifyRecordsOperation {
    @available(swift 4.2)
    public convenience init(recordsToSave: [CKRecord]? = nil, recordIDsToDelete: [CKRecord.ID]? = nil) {
        self.init()
        self.recordsToSave = recordsToSave
        self.recordIDsToDelete = recordIDsToDelete
    }
}

#if !os(watchOS)

@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKModifySubscriptionsOperation {
    @available(swift 4.2)
    public convenience init(subscriptionsToSave: [CKSubscription]? = nil, subscriptionIDsToDelete: [CKSubscription.ID]? = nil) {
        self.init()
        self.subscriptionsToSave = subscriptionsToSave
        self.__subscriptionIDsToDelete = subscriptionIDsToDelete
    }
}

#endif // os(watchOS)

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord {
    @available(swift, introduced: 4.2, deprecated: 4.2, message: "Use init(recordType:recordID:) + CKRecord.ID(zoneID:) instead")
    public convenience init(recordType: CKRecord.RecordType, zoneID: CKRecordZone.ID) {
        self.init(recordType: recordType, recordID: CKRecord.ID(zoneID: zoneID))
    }
    @available(swift 4.2)
    public convenience init(recordType: CKRecord.RecordType, recordID: CKRecord.ID = CKRecord.ID()) {
        self.init(__recordType: recordType, recordID: recordID)
    }
}

#if !os(watchOS)

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *) @available(watchOS, unavailable)
extension CKQuerySubscription {
    @available(swift 4.2)
    public convenience init(recordType: CKRecord.RecordType, predicate: NSPredicate, subscriptionID: CKSubscription.ID = UUID().uuidString, options querySubscriptionOptions: CKQuerySubscription.Options = []) {
        self.init(__recordType: recordType, predicate: predicate, subscriptionID: subscriptionID, options: querySubscriptionOptions)
    }
}

#endif // !os(watchOS)
