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


#if os(macOS) || os(iOS)
  
extension AVCaptureVideoDataOutput {
  @available(swift, obsoleted: 4.0)
  @available(macOS, introduced: 10.7)
  @available(iOS, introduced: 5.0)
  @nonobjc
  public var availableVideoCVPixelFormatTypes: [Any]! {
    return __availableVideoCVPixelFormatTypes
  }
  
  @available(swift, introduced: 4.0)
  @available(macOS, introduced: 10.7)
  @available(iOS, introduced: 5.0)
  @nonobjc
  public var availableVideoPixelFormatTypes: [OSType] {
    return __availableVideoCVPixelFormatTypes.map { $0.uint32Value } as [OSType]
  }
}
  
#endif
