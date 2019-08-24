// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

let AppKitTests = TestSuite("AppKit_Swift4")

AppKitTests.test("NSEventMaskFromType") {
  let eventType: NSEvent.EventType = .keyDown
  let eventMask = NSEvent.EventTypeMask(type: eventType)
  expectEqual(eventMask, .keyDown)
}

AppKitTests.test("NSWindowDepth.availableDepths") {
  let depths = NSWindow.Depth.availableDepths
  expectGT(depths.count, 0)
  for depth in depths {
    expectNotEqual(depth.rawValue, 0)
  }
}

AppKitTests.test("NSRectFills") {
  let bitmapImage = NSBitmapImageRep(
    bitmapDataPlanes: nil, pixelsWide: 3, pixelsHigh: 3,
    bitsPerSample: 8, samplesPerPixel: 4,
    hasAlpha: true, isPlanar: false,
    colorSpaceName: .deviceRGB,
    bytesPerRow: 0, bitsPerPixel: 0)!
  let graphicsContext = NSGraphicsContext(bitmapImageRep: bitmapImage)!
  NSGraphicsContext.saveGraphicsState()
  NSGraphicsContext.current = graphicsContext

  let canvas = NSRect(x: 0, y: 0, width: 3, height: 3)
  let bottomLeft = NSRect(x: 0, y: 0, width: 1, height: 1)
  let bottomCenter = NSRect(x: 1, y: 0, width: 1, height: 1)
  let bottomRight = NSRect(x: 2, y: 0, width: 1, height: 1)
  let middleCenter = NSRect(x: 1, y: 1, width: 1, height: 1)
  let middleRight = NSRect(x: 2, y: 1, width: 1, height: 1)
  let topLeft = NSRect(x: 0, y: 2, width: 1, height: 1)
  let topCenter = NSRect(x: 1, y: 2, width: 1, height: 1)
  let topRight = NSRect(x: 2, y: 2, width: 1, height: 1)
  let red = NSColor(deviceRed: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
  let green = NSColor(deviceRed: 0.0, green: 1.0, blue: 0.0, alpha: 1.0)
  let blue = NSColor(deviceRed: 0.0, green: 0.0, blue: 1.0, alpha: 1.0)
  let black = NSColor(deviceRed: 0.0, green: 0.0, blue: 0.0, alpha: 1.0)
  let white = NSColor(deviceRed: 1.0, green: 1.0, blue: 1.0, alpha: 1.0)

  // Blank out the canvas with white
  white.set()
  canvas.fill()

  // Fill the bottomLeft and middleRight with red using the Sequence<Rect> convenience
  red.set()
  [bottomLeft, middleRight].fill()

  // Fill the bottom right corner by clipping to it, and then filling the canvas
  NSGraphicsContext.saveGraphicsState()
  topRight.clip()
  black.set()
  canvas.fill()
  NSGraphicsContext.restoreGraphicsState()

  // Fill bottomRight and topLeft by clipping to them and filling a superset
  NSGraphicsContext.saveGraphicsState()
  [bottomRight, topLeft].clip()
  green.set()
  canvas.fill()
  blue.set()
  // effectively fill bottomRight only
  NSRect(x: 0, y: 0, width: 3, height: 1).fill()
  NSGraphicsContext.restoreGraphicsState()

  // Fill the center regions using the Sequence<(Rect, Color)> convenience
  [(topCenter, blue),
   (middleCenter, green),
   (bottomCenter, red)].fill()

  NSGraphicsContext.restoreGraphicsState()

  expectEqual(bitmapImage.colorAt(x: 0, y: 0), green)
  expectEqual(bitmapImage.colorAt(x: 1, y: 0), blue)
  expectEqual(bitmapImage.colorAt(x: 2, y: 0), black)
  expectEqual(bitmapImage.colorAt(x: 0, y: 1), white)
  expectEqual(bitmapImage.colorAt(x: 1, y: 1), green)
  expectEqual(bitmapImage.colorAt(x: 2, y: 1), red)
  expectEqual(bitmapImage.colorAt(x: 0, y: 2), red)
  expectEqual(bitmapImage.colorAt(x: 1, y: 2), red)
  expectEqual(bitmapImage.colorAt(x: 2, y: 2), blue)
}

AppKitTests.test("NSColor.Literals") {
  if #available(macOS 10.12, *) {
    // Color literal in the extended sRGB color space.
    let c1 = #colorLiteral(red: 1.358, green: -0.074, blue: -0.012, alpha: 1.0)
    
    var printedC1 = ""
    print(c1, to: &printedC1)
    expectTrue(printedC1.contains("extended"))
  }
}

AppKitTests.test("NSLayoutPriority") {
  let highPriority: NSLayoutConstraint.Priority = .defaultHigh

  let adjustedPriority1 = highPriority + 1
  let adjustedPriority1RawValue: Float = NSLayoutConstraint.Priority.defaultHigh.rawValue + 1
  expectEqual(adjustedPriority1.rawValue, adjustedPriority1RawValue)

  let adjustedPriority2 = highPriority - 5.0
  let adjustedPriority2RawValue: Float = NSLayoutConstraint.Priority.defaultHigh.rawValue - 5.0
  expectEqual(adjustedPriority2.rawValue, adjustedPriority2RawValue)

  let adjustedPriority3 = 5.0 + highPriority
  let adjustedPriority3RawValue: Float = 5.0 + NSLayoutConstraint.Priority.defaultHigh.rawValue
  expectEqual(adjustedPriority3.rawValue, adjustedPriority3RawValue)

  // Inferred typing from result type
  let adjustedPriority4: NSLayoutConstraint.Priority = .defaultHigh + 2.0
  let adjustedPriority4RawValue: Float = NSLayoutConstraint.Priority.defaultHigh.rawValue + 2.0
  expectEqual(adjustedPriority4.rawValue, adjustedPriority4RawValue)

  // Comparable
  expectTrue(adjustedPriority1 > adjustedPriority2)
  expectTrue(adjustedPriority2 < adjustedPriority1)

  // Compound assignment
  var variablePriority: NSLayoutConstraint.Priority = .defaultHigh
  variablePriority += 1
  variablePriority -= 5.0
  let variablePriorityRawValue: Float = NSLayoutConstraint.Priority.defaultHigh.rawValue + 1 - 5.0
  expectEqual(variablePriority.rawValue, variablePriorityRawValue)
}

runAllTests()
