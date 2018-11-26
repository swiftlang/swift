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


#if os(iOS)

@available(iOS, introduced: 10.0)
extension AVCapturePhotoOutput {
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var supportedFlashModes: [NSNumber] {
    return __supportedFlashModes
  }
  
  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var supportedFlashModes: [AVCaptureDevice.FlashMode] {
    return __supportedFlashModes.map { AVCaptureDevice.FlashMode(rawValue: $0.intValue)! }
  }
  
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availablePhotoPixelFormatTypes: [NSNumber] {
    return __availablePhotoPixelFormatTypes
  }
  
  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availablePhotoPixelFormatTypes: [OSType] {
    return __availablePhotoPixelFormatTypes.map { $0.uint32Value } as [OSType]
  }
  
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availableRawPhotoPixelFormatTypes: [NSNumber] {
    return __availableRawPhotoPixelFormatTypes
  }
  
  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availableRawPhotoPixelFormatTypes: [OSType] {
    return __availableRawPhotoPixelFormatTypes.map { $0.uint32Value } as [OSType]
  }
}

@available(iOS, introduced: 10.0)
extension AVCapturePhotoSettings {
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availablePreviewPhotoPixelFormatTypes: [NSNumber] {
    return __availablePreviewPhotoPixelFormatTypes
  }

  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var availablePreviewPhotoPixelFormatTypes: [OSType] {
    return __availablePreviewPhotoPixelFormatTypes.map { $0.uint32Value } as [OSType]
  }
}

#endif
