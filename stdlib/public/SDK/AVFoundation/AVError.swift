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
#if os(tvOS)
  @available(*, unavailable)
  public var device: String? { return nil }
#else
  @available(swift, obsoleted: 4.2, message: "Use `device: AVCaptureDevice?` instead")
  public var device: String? { return nil }

  @available(swift, introduced: 4.2)
  public var device: AVCaptureDevice? {
    return userInfo[AVErrorDeviceKey] as? AVCaptureDevice
  }
#endif

  /// The time.
  public var time: CMTime? {
    if let time = userInfo[AVErrorTimeKey] as? CMTime {
      return time
    }
    else if let timeDictionary = userInfo[AVErrorTimeKey] {
      return CMTimeMakeFromDictionary((timeDictionary as! CFDictionary))
    }
    else {
      return nil
    }
  }

  /// The file size.
  public var fileSize: Int64? {
    return userInfo[AVErrorFileSizeKey] as? Int64
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
  @available(swift, obsoleted: 4.2)
  public var mediaType: String? {
    return userInfo[AVErrorMediaTypeKey] as? String
  }

  /// The media type.
  @available(swift, introduced: 4.2)
  public var mediaType: AVMediaType? {
    return userInfo[AVErrorMediaTypeKey] as? AVMediaType
  }

  /// The media subtypes.
  public var mediaSubtypes: [Int]? {
    return userInfo[AVErrorMediaSubTypeKey] as? [Int]
  }

  /// The presentation time stamp.
  @available(swift, introduced: 4.2)
  @available(macOS, introduced: 10.10)
  @available(iOS, introduced: 8.0)
  @available(tvOS, introduced: 9.0)
  public var presentationTimeStamp: CMTime? {
    return userInfo[AVErrorPresentationTimeStampKey] as? CMTime
  }

  /// The persistent track ID.
  @available(swift, introduced: 4.2)
  @available(macOS, introduced: 10.10)
  @available(iOS, introduced: 8.0)
  @available(tvOS, introduced: 9.0)
  public var persistentTrackID: CMPersistentTrackID? {
    return userInfo[AVErrorPersistentTrackIDKey] as? CMPersistentTrackID
  }

  /// The file type.
  @available(swift, introduced: 4.2)
  @available(macOS, introduced: 10.10)
  @available(iOS, introduced: 8.0)
  @available(tvOS, introduced: 9.0)
  public var fileType: AVFileType? {
    return userInfo[AVErrorFileTypeKey] as? AVFileType
  }
}
