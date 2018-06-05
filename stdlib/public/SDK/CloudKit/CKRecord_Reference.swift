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
extension CKRecord.Reference {
    /// TODO: replace this with a .apinotes entry once there's a fix for 39261783
    @available(swift 4.2)
    public typealias Action = CKRecord_Reference_Action
}

