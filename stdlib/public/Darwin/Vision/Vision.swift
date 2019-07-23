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

@_exported import Vision
import Darwin

//===----------------------------------------------------------------------===//
// VNFaceLandmarkRegion2D
//===----------------------------------------------------------------------===//

#if !os(watchOS)

@available(macOS, introduced: 10.13)
@available(iOS, introduced: 11.0)
@available(tvOS, introduced: 11.0)
extension VNFaceLandmarkRegion2D {
  @nonobjc
  public var normalizedPoints: [CGPoint] {
    let pointsBuffer = UnsafeBufferPointer<CGPoint>(
      start: self.__normalizedPoints, count: Int(self.pointCount))
    return Array(pointsBuffer)
	}

  @nonobjc
  public func pointsInImage(imageSize: CGSize) -> [CGPoint] {
    let pointsBuffer = UnsafeBufferPointer<CGPoint>(
      start: self.__pointsInImage(imageSize: imageSize),
      count: Int(self.pointCount))
    return Array(pointsBuffer)
  }
}

#endif
