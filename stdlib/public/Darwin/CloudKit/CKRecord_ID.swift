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

/// A class coupling a record name and a record zone id
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord.ID {
    /**
     - parameter recordName: CKRecord names must be 255 characters or less. Most UTF-8 characters are valid.  Defaults to UUID().uuidString
     - parameter zoneID: defaults to the default record zone id
     */
    @available(swift 4.2)
    public convenience init(recordName: String = UUID().uuidString, zoneID: CKRecordZone.ID = CKRecordZone.ID.default) {
        self.init(__recordName: recordName, zoneID: zoneID)
    }
}
