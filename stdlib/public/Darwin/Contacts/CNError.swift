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

@_exported import Contacts
import Foundation

@available(macOS 10.11, iOS 9.0, *)
extension CNError {
  /// One or more CNContact, CNGroup or CNContainer objects for which
  /// the error applies.
  public var affectedRecords: [AnyObject]? {
    return userInfo[CNErrorUserInfoAffectedRecordsKey] as? [AnyObject]
  }

  /// The record identifiers to which this error applies.
  public var affectedRecordIdentifiers: [String]? {
    return userInfo[CNErrorUserInfoAffectedRecordIdentifiersKey] as? [String]
  }

  /// The key paths associated with a given error. For validation
  /// errors this will contain key paths to specific object
  /// properties.
  public var keyPaths: [String]? {
    return userInfo[CNErrorUserInfoKeyPathsKey] as? [String]
  }
}
