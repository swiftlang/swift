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

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
extension CKShare {
    @available(swift 4.2)
    public enum SystemFieldKey {
        public static let title: CKRecord.FieldKey = __CKShareTitleKey as CKRecord.FieldKey
        public static let thumbnailImageData: CKRecord.FieldKey = __CKShareThumbnailImageDataKey as CKRecord.FieldKey
        public static let shareType: CKRecord.FieldKey = __CKShareTypeKey as CKRecord.FieldKey
    }
}

