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

@_exported import AVFoundation // Clang module
import Foundation


extension AVError {
  /// The device name.
  public var device: String? {
    return userInfo[AVErrorDeviceKey] as? String
  }

  /// The time.
  public var time: CMTime? {
    return userInfo[AVErrorTimeKey] as? CMTime
  }

  /// The file size.
  public var fileSize: Int64? {
    return (userInfo[AVErrorFileSizeKey] as? NSNumber)?.int64Value
  }

  /// The process ID number.
  public var processID: Int? {
    return userInfo[AVErrorPIDKey] as? Int
  }

  /// Whether the recording successfully finished.
  public var recordingSuccessfullyFinished: Bool? {
    return userInfo[AVErrorRecordingSuccessfullyFinishedKey] as? Bool
  }

  /// The media type.
  public var mediaType: String? {
    return userInfo[AVErrorMediaTypeKey] as? String
  }

  /// The media subtypes.
  public var mediaSubtypes: [Int]? {
    return userInfo[AVErrorMediaSubTypeKey] as? [Int]
  }
}

