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
@available(macOS 10.12, iOS 9.3, tvOS 9.2, watchOS 3.0, *)
extension CKContainer {
     /**
     If a long lived operation is cancelled or finishes completely it is no longer returned by this call.
     */
    @available(swift 4.2)
    public func fetchAllLongLivedOperationIDs(completionHandler: @escaping ([CKOperation.ID]?, Error?) -> Void) {
        self.__fetchAllLongLivedOperationIDs(completionHandler: completionHandler)
    }
     /**
     Long lived CKOperations returned by this call must be started on an operation queue.
     Remember to set the callback blocks before starting the operation.
     If an operation has already completed against the server, and is subsequently resumed, that operation will replay all of its callbacks from the start of the operation, but the request will not be re-sent to the server.
     If a long lived operation is cancelled or finishes completely it is no longer returned by this call.
     */
    @available(swift 4.2)
    public func fetchLongLivedOperation(withID operationID: CKOperation.ID, completionHandler: @escaping (CKOperation?, Error?) -> Void) {
        self.__fetchLongLivedOperation(withID: operationID, completionHandler: completionHandler)
    }
}

@nonobjc
@available(macOS 10.12, iOS 9.3, tvOS 9.2, watchOS 3.0, *)
extension CKOperation {
    /** This is an identifier unique to this CKOperation.
     
     This value is chosen by the system, and will be unique to this instance of a CKOperation.  This identifier will be sent to Apple's servers, and can be used to identify any server-side logging associated with this operation.
     */
    @available(swift 4.2)
    public var operationID: CKOperation.ID {
        get { return self.__operationID as CKOperation.ID }
    }
}

#if !os(watchOS)

@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKModifySubscriptionsOperation {
    @available(swift 4.2)
    public var subscriptionIDsToDelete: [CKSubscription.ID]? {
        get { return self.__subscriptionIDsToDelete }
        set { self.__subscriptionIDsToDelete = newValue }
    }
    /** This block is called when the operation completes.
     
     The Operation.completionBlock will also be called if both are set.
     If the error is `CKErrorPartialFailure`, the error's userInfo dictionary contains a dictionary of subscriptionIDs to errors keyed off of `CKPartialErrorsByItemIDKey`.
     */
    @available(swift 4.2)
    public var modifySubscriptionsCompletionBlock: (([CKSubscription]?, [CKSubscription.ID]?, Error?) -> Void)? {
        get { return self.__modifySubscriptionsCompletionBlock }
        set { self.__modifySubscriptionsCompletionBlock = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKFetchSubscriptionsOperation {
    @available(swift 4.2)
    public convenience init(subscriptionIDs: [CKSubscription.ID]) {
        self.init(__subscriptionIDs: subscriptionIDs)
    }
    
    @available(swift 4.2)
    public var subscriptionIDs: [CKSubscription.ID]? {
        get { return self.__subscriptionIDs }
        set { self.__subscriptionIDs = newValue }
    }
    /** This block is called when the operation completes.
     
     The Operation.completionBlock will also be called if both are set.
     If the error is `CKErrorPartialFailure`, the error's userInfo dictionary contains a dictionary of subscriptionID to errors keyed off of `CKPartialErrorsByItemIDKey`.
     */
    @available(swift 4.2)
    public var fetchSubscriptionCompletionBlock: (([CKSubscription.ID : CKSubscription]?, Error?) -> Void)? {
        get { return self.__fetchSubscriptionCompletionBlock }
        set { self.__fetchSubscriptionCompletionBlock = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKDatabase {
    @available(swift 4.2)
    public func fetch(withSubscriptionID subscriptionID: CKSubscription.ID, completionHandler: @escaping (CKSubscription?, Error?) -> Void) {
        self.__fetch(withSubscriptionID: subscriptionID, completionHandler: completionHandler)
    }
    @available(swift 4.2)
    public func delete(withSubscriptionID subscriptionID: CKSubscription.ID, completionHandler: @escaping (String?, Error?) -> Void) {
        self.__delete(withSubscriptionID: subscriptionID, completionHandler: completionHandler)
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKSubscription {
    @available(swift 4.2)
    public var subscriptionID: CKSubscription.ID {
        get { return self.__subscriptionID }
    }
}

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *) @available(watchOS, unavailable)
extension CKQuerySubscription {
    /// The record type that this subscription watches
    @available(swift 4.2)
    public var recordType: CKRecord.RecordType? {
        get { return self.__recordType }
    }
}

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *) @available(watchOS, unavailable)
extension CKRecordZoneSubscription {
    @available(swift 4.2)
    public convenience init(zoneID: CKRecordZone.ID, subscriptionID: CKSubscription.ID) {
        self.init(__zoneID: zoneID, subscriptionID: subscriptionID)
    }
    /// Optional property. If set, a zone subscription is scoped to record changes for this record type
    @available(swift 4.2)
    public var recordType: CKRecord.RecordType? {
        get { return self.__recordType }
        set { self.__recordType = newValue }
    }
}

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, *) @available(watchOS, unavailable)
extension CKDatabaseSubscription {
    @available(swift 4.2)
    public convenience init(subscriptionID: CKSubscription.ID) {
        self.init(__subscriptionID: subscriptionID)
    }
    /// Optional property. If set, a database subscription is scoped to record changes for this record type
    @available(swift 4.2)
    public var recordType: CKRecord.RecordType? {
        get { return self.__recordType }
        set { self.__recordType = newValue }
    }
}

#endif // !os(watchOS)

@nonobjc
@available(macOS 10.11, iOS 9.0, watchOS 3.0, *)
extension CKNotification {
    /// The ID of the subscription that caused this notification to fire
    @available(swift 4.2)
    public var subscriptionID: CKSubscription.ID? {
        get { return self.__subscriptionID }
    }
}

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
extension CKFetchRecordZoneChangesOperation {
    @available(swift 4.2)
    public var recordWithIDWasDeletedBlock: ((CKRecord.ID, CKRecord.RecordType) -> Void)? {
        get { return self.__recordWithIDWasDeletedBlock }
        set { self.__recordWithIDWasDeletedBlock = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKQuery {
    /// Use `NSPredicate(value: true)` if you want to query for all records of a given type.
    @available(swift 4.2)
    public convenience init(recordType: CKRecord.RecordType, predicate: NSPredicate) {
        self.init(__recordType: recordType, predicate: predicate)
    }
    @available(swift 4.2)
    public var recordType: CKRecord.RecordType {
        get { return self.__recordType }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord {
    @available(swift 4.2)
    public var recordType: CKRecord.RecordType {
        get { return self.__recordType }
    }
    /** In addition to `object(forKey:)` and `setObject(_:forKey:)`, dictionary-style subscripting (`record[key]` and `record[key] = value`) can be used to get and set values.
     Acceptable value object classes are:
     - String
     - Date
     - Data
     - Bool
     - Int
     - UInt
     - Float
     - Double
     - [U]Int8 et al
     - CKReference / Record.Reference
     - CKAsset
     - CLLocation
     - NSData
     - NSDate
     - NSNumber
     - NSString
     - Array and/or NSArray containing objects of any of the types above
     
     Any other classes will result in an exception with name `NSInvalidArgumentException`.
     
     Field keys starting with '_' are reserved. Attempting to set a key prefixed with a '_' will result in an error.
     
     Key names roughly match C variable name restrictions. They must begin with an ASCII letter and can contain ASCII letters and numbers and the underscore character.
     The maximum key length is 255 characters.
     */
    @available(swift 4.2)
    public func object(forKey key: CKRecord.FieldKey) -> __CKRecordObjCValue? {
        return self.__object(forKey: key)
    }
    @available(swift 4.2)
    public func setObject(_ object: __CKRecordObjCValue?, forKey key: CKRecord.FieldKey) {
        self.__setObject(object, forKey: key)
    }
    @available(swift 4.2)
    public subscript(key: CKRecord.FieldKey) -> __CKRecordObjCValue? {
        get { return self.object(forKey: key) }
        set { self.setObject(newValue, forKey: key) }
    }
    @available(swift 4.2)
    public func allKeys() -> [CKRecord.FieldKey] {
        return self.__allKeys()
    }
    /// A list of keys that have been modified on the local CKRecord instance
    @available(swift 4.2)
    public func changedKeys() -> [CKRecord.FieldKey] {
        return self.__changedKeys()
    }
}

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
extension CKFetchShareMetadataOperation {
    /** Declares which user-defined keys should be fetched and added to the resulting `rootRecord`.
     
     Only consulted if `shouldFetchRootRecord` is YES.
     If nil, declares the entire root record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.
     Defaults to nil.
     */
    @available(swift 4.2)
    public var rootRecordDesiredKeys: [CKRecord.FieldKey]? {
        get { return self.__rootRecordDesiredKeys }
        set { self.__rootRecordDesiredKeys = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKFetchRecordsOperation {
    /** Declares which user-defined keys should be fetched and added to the resulting CKRecords.
     
     If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.
     Defaults to nil.
     */
    @available(swift 4.2)
    public var desiredKeys: [CKRecord.FieldKey]? {
        get { return self.__desiredKeys }
        set { self.__desiredKeys = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKQueryOperation {
    /** Declares which user-defined keys should be fetched and added to the resulting CKRecords.
     
     If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.
     Defaults to nil.
     */
    @available(swift 4.2)
    public var desiredKeys: [CKRecord.FieldKey]? {
        get { return self.__desiredKeys }
        set { self.__desiredKeys = newValue }
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKRecord.SystemType.userRecord")
public let CKRecordTypeUserRecord: String = __CKRecordTypeUserRecord

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKRecord.SystemFieldKey.parent")
public let CKRecordParentKey: String = __CKRecordParentKey

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKRecord.SystemFieldKey.share")
public let CKRecordShareKey: String = __CKRecordShareKey



@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKRecord.SystemType.share")
public let CKRecordTypeShare: String = __CKRecordTypeShare

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKShare.SystemFieldKey.title")
public let CKShareTitleKey: String = __CKShareTitleKey

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKShare.SystemFieldKey.thumbnailImageData")
public let CKShareThumbnailImageDataKey: String = __CKShareThumbnailImageDataKey

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKShare.SystemFieldKey.shareType")
public let CKShareTypeKey: String = __CKShareTypeKey


