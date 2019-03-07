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
extension CKFetchRecordZoneChangesOperation.ZoneConfiguration {
    /**
     Declares which user-defined keys should be fetched and added to the resulting CKRecords.
     
     If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.
     Defaults to nil.
     */
    @available(swift 4.2)
    public var desiredKeys: [CKRecord.FieldKey]? {
        get { return self.__desiredKeys }
        set { self.__desiredKeys = newValue }
    }
    
    @available(swift 4.2)
    public convenience init(previousServerChangeToken: CKServerChangeToken? = nil, resultsLimit: Int? = nil, desiredKeys: [CKRecord.FieldKey]? = nil) {
        self.init()
        if let previousServerChangeToken = previousServerChangeToken {
            self.previousServerChangeToken = previousServerChangeToken
        }
        if let resultsLimit = resultsLimit {
            self.resultsLimit = resultsLimit
        }
        if let desiredKeys = desiredKeys {
            self.desiredKeys = desiredKeys
        }
    }
}
