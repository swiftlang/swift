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

@_exported import Foundation // Clang module

/// Decorates types which are backed by a Foundation reference type.
///
/// All `ReferenceConvertible` types are hashable, equatable, and provide description functions.
public protocol ReferenceConvertible : _ObjectiveCBridgeable, CustomStringConvertible, CustomDebugStringConvertible, Hashable {
    associatedtype ReferenceType : NSObject, NSCopying
}
