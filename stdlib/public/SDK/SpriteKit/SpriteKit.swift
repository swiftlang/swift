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
  /* init with the specified dimensions, source and dest positions. */
  public convenience init(columns: Int, rows: Int, sourcePositions: [simd.float2]? = nil, destinationPositions: [simd.float2]? = nil) {
     self.init(__columns: columns, rows: rows, sourcePositions: sourcePositions, destPositions:destinationPositions)
  }
  public func replacingBySourcePositions(positions source: [simd.float2]) -> SKWarpGeometryGrid {
    return self.__replacingSourcePositions(source)
  }
  public func replacingByDestinationPositions(positions destination: [simd.float2]) -> SKWarpGeometryGrid {
    return self.__replacingDestPositions(destination)
  }
}
