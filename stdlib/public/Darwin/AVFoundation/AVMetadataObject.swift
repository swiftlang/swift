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
import CoreGraphics


#if os(iOS)
  
extension AVMetadataMachineReadableCodeObject {
  @available(swift, obsoleted: 4.0)
  @available(iOS, introduced: 7.0)
  @nonobjc
  public var corners: [Any]! {
    return __corners
  }
  
  @available(swift, introduced: 4.0)
  @available(iOS, introduced: 7.0)
  @nonobjc
  public var corners: [CGPoint] {
    return __corners.map { CGPoint(dictionaryRepresentation: $0 as CFDictionary)! }
  }
}
  
#endif

