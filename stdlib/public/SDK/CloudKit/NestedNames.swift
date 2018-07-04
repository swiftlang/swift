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

// Work arounds for 39295365, 39261783

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
public extension CKContainer {
    @available(swift 4.2)
    public enum Application {
        public typealias Permissions = CKContainer_Application_Permissions
        public typealias PermissionStatus = CKContainer_Application_PermissionStatus
        public typealias PermissionBlock = CKContainer_Application_PermissionBlock
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKOperation {
    @available(swift 4.2)
    public typealias ID = String
}

#if os(watchOS)

@available(watchOS 3.0, *)
public enum CKSubscription {}

#endif // os(watchOS)

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKSubscription {
    @available(swift 4.2)
    public typealias ID = String
}
