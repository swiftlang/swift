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

@_exported import SpriteKit
import simd

// SpriteKit defines SKColor using a macro.

#if os(OSX)
public typealias SKColor = NSColor
#elseif os(iOS) || os(tvOS) || os(watchOS)
public typealias SKColor = UIColor
#endif

// this class only exists to allow AnyObject lookup of _copyImageData
// since that method only exists in a private header in SpriteKit, the lookup
// mechanism by default fails to accept it as a valid AnyObject call
@objc class _SpriteKitMethodProvider : NSObject {
  override init() { _sanityCheckFailure("don't touch me") }
  @objc func _copyImageData() -> NSData! { return nil }
}

@available(iOS, introduced: 10.0)
@available(OSX, introduced: 10.12)
@available(tvOS, introduced: 10.0)
@available(watchOS, introduced: 3.0)
extension SKWarpGeometryGrid {
  /// Create a grid of the specified dimensions, source and destination positions.
  ///
  /// Grid dimensions (columns and rows) refer to the number of faces in each dimension. The
  /// number of vertices required for a given dimension is equal to (cols + 1) * (rows + 1).
  ///
  /// SourcePositions are normalized (0.0 - 1.0) coordinates to determine the source content.
  ///
  /// DestinationPositions are normalized (0.0 - 1.0) positional coordinates with respect to
  /// the node's native size. Values outside the (0.0-1.0) range are perfectly valid and
  /// correspond to positions outside of the native undistorted bounds.
  ///
  /// Source and destination positions are provided in row-major order starting from the top-left.
  /// For example the indices for a 2x2 grid would be as follows:
  ///
  ///     [0]---[1]---[2]
  ///      |     |     |
  ///     [3]---[4]---[5]
  ///      |     |     |
  ///     [6]---[7]---[8]
  ///
  /// - Parameter columns: the number of columns to initialize the SKWarpGeometryGrid with
  /// - Parameter rows: the number of rows to initialize the SKWarpGeometryGrid with
  /// - Parameter sourcePositions: the source positions for the SKWarpGeometryGrid to warp from
  /// - Parameter destinationPositions: the destination positions for SKWarpGeometryGrid to warp to
  public convenience init(columns: Int, rows: Int, sourcePositions: [simd.float2] = [float2](), destinationPositions: [simd.float2] = [float2]()) {
    let requiredElementsCount = (columns + 1) * (rows + 1)
    switch (destinationPositions.count, sourcePositions.count) {
    case (0, 0):
        self.init(__columns: columns, rows: rows, sourcePositions: nil, destPositions: nil)
    case (let dests, 0):
        _precondition(dests == requiredElementsCount, "Mismatch found between rows/columns and positions.")
        self.init(__columns: columns, rows: rows, sourcePositions: nil, destPositions: destinationPositions)
    case (0, let sources):
        _precondition(sources == requiredElementsCount, "Mismatch found between rows/columns and positions.")
        self.init(__columns: columns, rows: rows, sourcePositions: sourcePositions, destPositions: nil)
    case (let dests, let sources):
        _precondition(dests == requiredElementsCount && sources == requiredElementsCount, "Mismatch found between rows/columns and positions.")
        self.init(__columns: columns, rows: rows, sourcePositions: sourcePositions, destPositions: destinationPositions)
    }
  }

  public func replacingBySourcePositions(positions source: [simd.float2]) -> SKWarpGeometryGrid {
    return self.__replacingSourcePositions(source)
  }

  public func replacingByDestinationPositions(positions destination: [simd.float2]) -> SKWarpGeometryGrid {
    return self.__replacingDestPositions(destination)
  }
}
