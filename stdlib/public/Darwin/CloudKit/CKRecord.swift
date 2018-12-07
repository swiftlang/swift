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

@_exported import CloudKit

// MARK: - Iterate over records

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
public struct CKRecordKeyValueIterator : IteratorProtocol {
    private var keyArray: [CKRecord.FieldKey]
    private var record: CKRecord
    private var index: Array<CKRecord.FieldKey>.Index
    
    fileprivate init(_ record: CKRecord) {
        self.record = record
        keyArray = record.allKeys()
        index = keyArray.startIndex
    }

    public mutating func next() -> (CKRecord.FieldKey, CKRecordValueProtocol)? {
        var key: CKRecord.FieldKey? = nil
        var objcValue: __CKRecordObjCValue? = nil
        while objcValue == nil {
            guard index < keyArray.endIndex else { return nil }
            key = keyArray[index]
            objcValue = record[key!]
            index = index.advanced(by: 1)
        }
        
        let swiftValue = objcValue!.asSwiftNativeValue()

        return (key!, swiftValue)
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord : Sequence {
    public func makeIterator() -> CKRecordKeyValueIterator {
        return CKRecordKeyValueIterator(self)
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord {
    public typealias RecordType = String
    public typealias FieldKey = String

    @available(swift 4.2)
    public enum SystemType {
        public static let userRecord: CKRecord.RecordType = __CKRecordTypeUserRecord as CKRecord.RecordType
        @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
        public static let share: CKRecord.RecordType = __CKRecordTypeShare as CKRecord.RecordType
    }
    
    @available(swift 4.2)
    public enum SystemFieldKey {
        @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
        public static let parent: CKRecord.FieldKey = __CKRecordParentKey as CKRecord.RecordType
        @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
        public static let share: CKRecord.FieldKey = __CKRecordShareKey as CKRecord.RecordType
    }
}

