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
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecordZone.ID {
    /**
     - parameter zoneName: Zone names must be 255 characters or less. Most UTF-8 characters are valid.  Defaults to CKRecordZone.ID.defaultZoneName
     - parameter ownerName: defaults to CurrentUserDefaultName
     */
    public convenience init(zoneName: String = CKRecordZone.ID.defaultZoneName, ownerName: String = "__defaultOwner__") {
        // CKCurrentUserDefaultName would be preferable to __defaultOwner__, but that only came around in macOS 10.12 and friends.
        self.init(__zoneName: zoneName, ownerName: ownerName)
    }
    
    public static let `default` = CKRecordZone.ID(zoneName: CKRecordZone.ID.defaultZoneName, ownerName: "__defaultOwner__")
    // CKCurrentUserDefaultName would be preferable to __defaultOwner__, but that only came around in macOS 10.12 and friends.
    public static let defaultZoneName = __CKRecordZoneDefaultName
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
@available(swift, obsoleted: 4.2, renamed: "CKRecordZone.ID.defaultZoneName")
public let CKRecordZoneDefaultName: String = __CKRecordZoneDefaultName


