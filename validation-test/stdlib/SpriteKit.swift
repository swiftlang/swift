// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest

import Foundation
import SpriteKit

var SpriteKitTests = TestSuite("SpriteKit")

// Check that the subscript is there.
@available(OSX,introduced: 10.10)
@available(iOS,introduced: 8.0)
@available(tvOS,introduced: 8.0)
@available(watchOS,introduced: 2.0)
func testSubscript(_ node: SKNode) {
  var result = node["me"]
  expectType(Array<SKNode>.self, &result)
}

SpriteKitTests.test("SKColor/TypeEquivalence") {
  // SKColor is NSColor on OS X and UIColor on iOS.
#if os(OSX)
  expectEqualType(NSColor.self, SKColor.self)
#elseif os(iOS) || os(tvOS) || os(watchOS)
  expectEqualType(UIColor.self, SKColor.self)
#else
  _UnknownOSError()
#endif
}

SpriteKitTests.test("getRed(_:green:blue:alpha:)") {
  var r: CGFloat = 0.0
  var g: CGFloat = 0.0
  var b: CGFloat = 0.0
  var a: CGFloat = 0.0
  let color = SKColor.red
  color.getRed(&r, green: &g, blue: &b, alpha: &a)
  expectEqual(1.0, r)
  expectEqual(0.0, g)
  expectEqual(0.0, b)
  expectEqual(1.0, a)
}

if #available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *) {
  SpriteKitTests.test("SKNode.setValue(_:forAttribute:)") {
    let node = SKNode()
    let attrVal = SKAttributeValue(float: 2.0)
    node.setValue(attrVal, forAttribute: "test")
    expectEqual(node.attributeValues["test"], attrVal)
  }

  SpriteKitTests.test("SKWarpGeometryGrid/1") {
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

    warpGrid = SKWarpGeometryGrid(columns: 1, rows: 1, sourcePositions: [float2(2.0), float2(1.0), float2(3.0), float2(4.0)], destinationPositions: [float2(1.0), float2(2.0), float2(3.0), float2(4.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 1.0)
    expectEqual(warpGrid.sourcePosition(at: 0).x, 2.0)
  }

  SpriteKitTests.test("SKWarpGeometryGrid/2") {
    var warpGrid = SKWarpGeometryGrid(columns: 3, rows: 4)
    expectEqual(warpGrid.numberOfColumns, 3)

    expectEqual(warpGrid.sourcePosition(at: 0).x, 0.0)
    warpGrid = warpGrid.replacingBySourcePositions(positions: [float2(30.0)])
    expectEqual(warpGrid.sourcePosition(at: 0).x, 30.0)

    expectEqual(warpGrid.destPosition(at: 0).x, 0.0)
    warpGrid = warpGrid.replacingByDestinationPositions(positions: [float2(30.0)])
    expectEqual(warpGrid.destPosition(at: 0).x, 30.0)
  }
}

runAllTests()
