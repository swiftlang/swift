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
extension AVCaptureDevice.Format {
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var supportedColorSpaces: [NSNumber]! {
    return __supportedColorSpaces
  }
  
  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 10.0)
  @nonobjc
  public var supportedColorSpaces: [AVCaptureColorSpace] {
    return __supportedColorSpaces.map { AVCaptureColorSpace(rawValue: $0.intValue)! }
  }
}

#endif

