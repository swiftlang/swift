// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest

import Foundation
import SpriteKit

// Check that the subscript is there.
@available(OSX,introduced: 10.10)
@available(iOS,introduced: 8.0)
@available(tvOS,introduced: 8.0)
@available(watchOS,introduced: 2.0)
func testSubscript(_ node: SKNode) {
  var _: [SKNode] = node["me"]
}

// SKColor is NSColor on OS X and UIColor on iOS.

var r = CGFloat(0)
var g = CGFloat(0)
var b = CGFloat(0)
var a = CGFloat(0)
var color = SKColor.red()
color.getRed(&r, green:&g, blue:&b, alpha:&a)
print("color \(r) \(g) \(b) \(a)")
// CHECK: color 1.0 0.0 0.0 1.0

#if os(OSX)
func f(_ c: NSColor) {
  print("colortastic")
}
#elseif os(iOS) || os(tvOS) || os(watchOS)
func f(_ c: UIColor) {
  print("colortastic")
}
#endif
f(color)
// CHECK: colortastic

var SpriteKitTests = TestSuite("SpriteKit")

if #available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *) {
  SpriteKitTests.test("SKNode.setValue(_:forAttribute:)") {
    let node = SKNode()
    let attrVal = SKAttributeValue(float: 2.0)
    node.setValue(attrVal, forAttribute: "test")
    expectEqual(node.attributeValues["test"], attrVal)
  }

  SpriteKitTests.test("SKWarpGeometryGrid") {
    var warpGrid = SKWarpGeometryGrid(columns: 1, rows: 1)
    expectEqual(warpGrid.numberOfColumns, 1) 

    expectEqual(warpGrid.sourcePosition(at: 0).x, 0.0)
    warpGrid = warpGrid.replacingBySourcePositions(positions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.sourcePosition(at: 0).x, 1.0)

    expectEqual(warpGrid.destPosition(at: 0).x, 0.0)
    warpGrid = warpGrid.replacingByDestinationPositions(positions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 1.0)

    warpGrid = SKWarpGeometryGrid(columns: 1, rows: 1, destinationPositions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 1.0)
    expectEqual(warpGrid.sourcePosition(at: 0).x, 0.0)

    warpGrid = SKWarpGeometryGrid(columns: 1, rows: 1, sourcePositions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 0.0)
    expectEqual(warpGrid.sourcePosition(at: 0).x, 1.0)

    warpGrid = SKWarpGeometryGrid(columns: 1, rows: 1, destinationPositions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)], sourcePositions: [float2(2.0), float2(1.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 1.0)
    expectEqual(warpGrid.sourcePosition(at: 0).x, 2.0)
  }
}

runAllTests()
