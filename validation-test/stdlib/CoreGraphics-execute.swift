// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import CoreGraphics
import StdlibUnittest

let CoreGraphicsTests = TestSuite("CoreGraphics")

//===----------------------------------------------------------------------===//
// CGAffineTransform
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGAffineTransform/Equatable") {
  checkEquatable([
    CGAffineTransform(a: 1, b: 0, c: 0, d: 1, tx: 0, ty: 0),
    CGAffineTransform.identity,
    CGAffineTransform(a: 1, b: 10, c: 10, d: 1, tx: 0, ty: 0),
    CGAffineTransform(a: 1, b: 10, c: 10, d: 1, tx: 0, ty: 0),
  ] as [CGAffineTransform],
  oracle: { $0 / 2 == $1 / 2 })
}

//===----------------------------------------------------------------------===//
// CGColor
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGColor/Equatable") {
  checkEquatable([
    CGColor(red: 1, green: 0, blue: 0, alpha: 1),
    CGColor(red: 1, green: 0, blue: 0, alpha: 0),
    CGColor(red: 0, green: 1, blue: 0, alpha: 1),
    CGColor(red: 0, green: 1, blue: 0, alpha: 0),
    CGColor(red: 0, green: 0, blue: 1, alpha: 1),
    CGColor(red: 0, green: 0, blue: 1, alpha: 0),
  ] as [CGColor],
  oracle: { $0 == $1 })
}

CoreGraphicsTests.test("CGColor.components") {
  let red = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
  let components = red.components!
  expectEqual(components.count, 4)
  expectEqual(components[0], 1)
  expectEqual(components[1], 0)
  expectEqual(components[2], 0)
  expectEqual(components[3], 1)
}

CoreGraphicsTests.test("CGColor/ExpressibleByColorLiteral") {
  let colorLit: CGColor = #colorLiteral(red: 0.25, green: 0.5, blue: 0.75,
                                        alpha: 1.0)
  let components = colorLit.components!
  expectEqual(components.count, 4)
  expectEqual(components[0], 0.25)
  expectEqual(components[1], 0.50)
  expectEqual(components[2], 0.75)
  expectEqual(components[3], 1.0)
}

//===----------------------------------------------------------------------===//
// CGPoint
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGPoint/Equatable") {
  checkEquatable([
    CGPoint(x: 0, y: 0),

    CGPoint(x: -1, y: -1),
    CGPoint(x: -1, y: 0),
    CGPoint(x: 0, y: -1),

    CGPoint(x: 1, y: 1),
    CGPoint(x: 1, y: 0),
    CGPoint(x: 0, y: 1),

    CGPoint(x: 1.nextUp, y: 1.nextUp),
    CGPoint(x: 1.nextUp, y: 0),
    CGPoint(x: 0, y: 1.nextUp),

    CGPoint(x: CGFloat.greatestFiniteMagnitude, y: 0),
  ] as [CGPoint],
  oracle: { $0 == $1 })
}

CoreGraphicsTests.test("CGPoint.init(x:y:)") {
  var fractional = CGPoint()
  fractional.x = 1.25
  fractional.y = 2.25

  var negativeFractional = CGPoint()
  negativeFractional.x = -1.25
  negativeFractional.y = -2.25

  var integral = CGPoint()
  integral.x = 1.0
  integral.y = 2.0

  var negativeIntegral = CGPoint()
  negativeIntegral.x = -1.0
  negativeIntegral.y = -2.0

  // Initialize from floating point literals.
  expectEqual(fractional, CGPoint(x: 1.25, y: 2.25))
  expectEqual(negativeFractional, CGPoint(x: -1.25, y: -2.25))

  // Initialize from integer literals.
  expectEqual(integral, CGPoint(x: 1, y: 2))
  expectEqual(negativeIntegral, CGPoint(x: -1, y: -2))

  expectEqual(fractional, CGPoint(x: 1.25 as CGFloat, y: 2.25 as CGFloat))
  expectEqual(fractional, CGPoint(x: 1.25 as Double, y: 2.25 as Double))
  expectEqual(integral, CGPoint(x: 1 as Int, y: 2 as Int))
}

CoreGraphicsTests.test("CGPoint.dictionaryRepresentation, CGPoint.init(dictionaryRepresentation:)") {
  let point = CGPoint(x: 1, y: 2)
  let dict = point.dictionaryRepresentation
  let newPoint = CGPoint(dictionaryRepresentation: dict)
  expectEqual(point, newPoint)
}

CoreGraphicsTests.test("CGPoint.zero") {
  expectEqual(0.0, CGPoint.zero.x)
  expectEqual(0.0, CGPoint.zero.y)
}

//===----------------------------------------------------------------------===//
// CGSize
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGSize/Equatable") {
  checkEquatable([
    CGSize(width: 0, height: 0),

    CGSize(width: -1, height: -1),
    CGSize(width: -1, height: 0),
    CGSize(width: 0, height: -1),

    CGSize(width: 1, height: 1),
    CGSize(width: 1, height: 0),
    CGSize(width: 0, height: 1),

    CGSize(width: 1.nextUp, height: 1.nextUp),
    CGSize(width: 1.nextUp, height: 0),
    CGSize(width: 0, height: 1.nextUp),

    CGSize(width: CGFloat.greatestFiniteMagnitude, height: 0),
  ] as [CGSize],
  oracle: { $0 == $1 })
}

CoreGraphicsTests.test("CGSize.init(width:height:)") {
  var fractional = CGSize()
  fractional.width = 1.25
  fractional.height = 2.25

  var negativeFractional = CGSize()
  negativeFractional.width = -1.25
  negativeFractional.height = -2.25

  var integral = CGSize()
  integral.width = 1.0
  integral.height = 2.0

  var negativeIntegral = CGSize()
  negativeIntegral.width = -1.0
  negativeIntegral.height = -2.0

  // Initialize from floating point literals.
  expectEqual(fractional, CGSize(width: 1.25, height: 2.25))
  expectEqual(negativeFractional, CGSize(width: -1.25, height: -2.25))

  // Initialize from integer literals.
  expectEqual(integral, CGSize(width: 1, height: 2))
  expectEqual(negativeIntegral, CGSize(width: -1, height: -2))

  expectEqual(fractional, CGSize(width: 1.25 as CGFloat, height: 2.25 as CGFloat))
  expectEqual(fractional, CGSize(width: 1.25 as Double, height: 2.25 as Double))
  expectEqual(integral, CGSize(width: 1 as Int, height: 2 as Int))
}

CoreGraphicsTests.test("CGSize.dictionaryRepresentation, CGSize.init(dictionaryRepresentation:)") {
  let size = CGSize(width: 3, height: 4)
  let dict = size.dictionaryRepresentation
  let newSize = CGSize(dictionaryRepresentation: dict)
  expectEqual(size, newSize)
}

CoreGraphicsTests.test("CGSize.zero") {
  expectEqual(0.0, CGSize.zero.width)
  expectEqual(0.0, CGSize.zero.height)
}

//===----------------------------------------------------------------------===//
// CGRect
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGRect/Equatable") {
  checkEquatable([
    CGRect.null,
    CGRect(x: 0, y: 0, width: 0, height: 0),

    CGRect(x: 1.25, y: 2.25, width: 3.25, height: 4.25),
    CGRect(x: -1.25, y: -2.25, width: -3.25, height: -4.25),

    CGRect(x: 1, y: 2, width: 3, height: 4),
    CGRect(x: -1, y: -2, width: -3, height: -4),
  ] as [CGRect],
  oracle: { $0 == $1 })
}

CoreGraphicsTests.test("CGRect.init(x:y:width:height:)") {
  var fractional = CGRect()
  fractional.origin = CGPoint(x: 1.25, y: 2.25)
  fractional.size = CGSize(width: 3.25, height: 4.25)

  var negativeFractional = CGRect()
  negativeFractional.origin = CGPoint(x: -1.25, y: -2.25)
  negativeFractional.size = CGSize(width: -3.25, height: -4.25)

  var integral = CGRect()
  integral.origin = CGPoint(x: 1.0, y: 2.0)
  integral.size = CGSize(width: 3.0, height: 4.0)

  var negativeIntegral = CGRect()
  negativeIntegral.origin = CGPoint(x: -1.0, y: -2.0)
  negativeIntegral.size = CGSize(width: -3.0, height: -4.0)

  // Initialize from floating point literals.
  expectEqual(fractional, CGRect(x: 1.25, y: 2.25, width: 3.25, height: 4.25))
  expectEqual(
    negativeFractional,
    CGRect(x: -1.25, y: -2.25, width: -3.25, height: -4.25))

  // Initialize from integer literals.
  expectEqual(integral, CGRect(x: 1, y: 2, width: 3, height: 4))
  expectEqual(negativeIntegral, CGRect(x: -1, y: -2, width: -3, height: -4))

  expectEqual(
    fractional,
    CGRect(
      x: 1.25 as CGFloat, y: 2.25 as CGFloat,
      width: 3.25 as CGFloat, height: 4.25 as CGFloat))
  expectEqual(
    fractional,
    CGRect(
      x: 1.25 as Double, y: 2.25 as Double,
      width: 3.25 as Double, height: 4.25 as Double))
  expectEqual(
    integral,
    CGRect(
      x: 1 as Int, y: 2 as Int,
      width: 3 as Int, height: 4 as Int))
}

CoreGraphicsTests.test("CGRect.init(origin:size:)") {
  let point = CGPoint(x: 1.25, y: 2.25)
  let size = CGSize(width: 3.25, height: 4.25)
  expectEqual(
    CGRect(x: 1.25, y: 2.25, width: 3.25, height: 4.25),
    CGRect(origin: point, size: size))
}

CoreGraphicsTests.test("CGRect.dictionaryRepresentation, CGRect.init(dictionaryRepresentation:)") {
  let point = CGPoint(x: 1, y: 2)
  let size = CGSize(width: 3, height: 4)
  let rect = CGRect(origin: point, size: size)
  let dict = rect.dictionaryRepresentation
  let newRect = CGRect(dictionaryRepresentation: dict)
  expectEqual(rect, newRect)
}

CoreGraphicsTests.test("CGRect.isNull") {
  expectFalse(CGRect.infinite.isNull)
  expectTrue(CGRect.null.isNull)
  expectFalse(CGRect.zero.isNull)
  expectFalse(CGRect(x: 0, y: 0, width: 10, height: 20).isNull)
}

CoreGraphicsTests.test("CGRect.isEmpty") {
  expectFalse(CGRect.infinite.isEmpty)
  expectTrue(CGRect.null.isEmpty)
  expectTrue(CGRect.zero.isEmpty)
  expectFalse(CGRect(x: 0, y: 0, width: 10, height: 20).isEmpty)
}

CoreGraphicsTests.test("CGRect.isInfinite") {
  expectTrue(CGRect.infinite.isInfinite)
  expectFalse(CGRect.null.isInfinite)
  expectFalse(CGRect.zero.isInfinite)
  expectFalse(CGRect(x: 0, y: 0, width: 10, height: 20).isInfinite)
}

CoreGraphicsTests.test("CGRect.contains(CGPoint)") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  expectTrue(rect.contains(CGPoint(x: 15, y: 25)))
  expectFalse(rect.contains(CGPoint(x: -15, y: 25)))
}

CoreGraphicsTests.test("CGRect.contains(CGRect)") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  let bigRect = CGRect(x: 1, y: 2, width: 101, height: 102)
  expectTrue(bigRect.contains(rect))
  expectFalse(rect.contains(bigRect))
}

CoreGraphicsTests.test("CGRect.divided()") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  let (slice, remainder) =
    rect.divided(atDistance: 5, from: CGRectEdge.minXEdge)
  expectEqual(CGRect(x: 11.25, y: 22.25, width: 5.0, height: 44.25), slice)
  expectEqual(CGRect(x: 16.25, y: 22.25, width: 28.25, height: 44.25), remainder)
}

CoreGraphicsTests.test("CGRect.standardized") {
  var unstandard = CGRect(x: 10, y: 20, width: -30, height: -50)
  var standard = unstandard.standardized

  expectEqual(CGPoint(x: 10, y: 20), unstandard.origin)
  expectEqual(CGPoint(x: -20, y: -30), standard.origin)

  expectEqual(CGSize(width: -30, height: -50), unstandard.size)
  expectEqual(CGSize(width: 30, height: 50), standard.size)

  expectEqual(unstandard, standard)
  expectEqual(standard, standard.standardized)

  expectEqual(30, unstandard.width)
  expectEqual(30, standard.width)

  expectEqual(50, unstandard.height)
  expectEqual(50, standard.height)

  expectEqual(-20, unstandard.minX)
  expectEqual(-5, unstandard.midX)
  expectEqual(10, unstandard.maxX)

  expectEqual(-20, standard.minX)
  expectEqual(-5, standard.midX)
  expectEqual(10, standard.maxX)

  expectEqual(-30, unstandard.minY)
  expectEqual(-5, unstandard.midY)
  expectEqual(20, unstandard.maxY)

  expectEqual(-30, standard.minY)
  expectEqual(-5, standard.midY)
  expectEqual(20, standard.maxY)
}

CoreGraphicsTests.test("CGRect.insetBy(self:dx:dy:)") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  expectEqual(
    CGRect(x: 12.25, y: 20.25, width: 31.25, height: 48.25),
    rect.insetBy(dx: 1, dy: -2))
}

CoreGraphicsTests.test("CGRect.offsetBy(self:dx:dy:)") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  expectEqual(
    CGRect(x: 14.25, y: 18.25, width: 33.25, height: 44.25),
    rect.offsetBy(dx: 3, dy: -4))
}

CoreGraphicsTests.test("CGRect.integral") {
  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)
  expectEqual(
    CGRect(x: 11, y: 22, width: 34, height: 45),
    rect.integral)
}

CoreGraphicsTests.test("CGRect.union(_:)") {
  let smallRect = CGRect(x: 10, y: 25, width: 5, height: -5)
  let bigRect = CGRect(x: 1, y: 2, width: 101, height: 102)
  let distantRect = CGRect(x: 1000, y: 2000, width: 1, height: 1)

  let rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)

  expectEqual(
    CGRect(x: 10.0, y: 20.0, width: 34.5, height: 46.5),
    rect.union(smallRect))
  expectEqual(
    CGRect(x: 1.0, y: 2.0, width: 101.0, height: 102.0),
    rect.union(bigRect))
  expectEqual(
    CGRect(x: 11.25, y: 22.25, width: 989.75, height: 1978.75),
    rect.union(distantRect))

  expectEqual(
    CGRect(x: 1.0, y: 2.0, width: 1000.0, height: 1999.0),
    rect.union(smallRect).union(bigRect).union(distantRect))
}

CoreGraphicsTests.test("CGRect.intersection(_:)") {
  let smallRect = CGRect(x: 10, y: 25, width: 5, height: -5)
  let bigRect = CGRect(x: 1, y: 2, width: 101, height: 102)
  let distantRect = CGRect(x: 1000, y: 2000, width: 1, height: 1)

  var rect = CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25)

  expectTrue(rect.intersects(smallRect))
  expectEqual(
    CGRect(x: 11.25, y: 22.25, width: 3.75, height: 2.75),
    rect.intersection(smallRect))

  expectTrue(rect.intersects(bigRect))
  expectEqual(
    CGRect(x: 11.25, y: 22.25, width: 33.25, height: 44.25),
    rect.intersection(bigRect))

  expectFalse(rect.intersects(distantRect))
  expectEqual(CGRect.null, rect.intersection(distantRect))

  expectFalse(
    rect
      .intersection(smallRect)
      .intersection(bigRect)
      .isEmpty)

  expectTrue(
    rect
      .intersection(smallRect)
      .intersection(bigRect)
      .intersection(distantRect)
      .isEmpty)
}

//===----------------------------------------------------------------------===//
// CGVector
//===----------------------------------------------------------------------===//

CoreGraphicsTests.test("CGVector/Equatable") {
  checkEquatable([
    CGVector(dx: 0, dy: 0),

    CGVector(dx: -1, dy: -1),
    CGVector(dx: -1, dy: 0),
    CGVector(dx: 0, dy: -1),

    CGVector(dx: 1, dy: 1),
    CGVector(dx: 1, dy: 0),
    CGVector(dx: 0, dy: 1),

    CGVector(dx: 1.nextUp, dy: 1.nextUp),
    CGVector(dx: 1.nextUp, dy: 0),
    CGVector(dx: 0, dy: 1.nextUp),

    CGVector(dx: CGFloat.greatestFiniteMagnitude, dy: 0),
  ] as [CGVector],
  oracle: { $0 == $1 })
}

CoreGraphicsTests.test("CGVector.init(dx:dy:)") {
  var fractional = CGVector()
  fractional.dx = 1.25
  fractional.dy = 2.25

  var negativeFractional = CGVector()
  negativeFractional.dx = -1.25
  negativeFractional.dy = -2.25

  var integral = CGVector()
  integral.dx = 1.0
  integral.dy = 2.0

  var negativeIntegral = CGVector()
  negativeIntegral.dx = -1.0
  negativeIntegral.dy = -2.0

  // Initialize from floating point literals.
  expectEqual(fractional, CGVector(dx: 1.25, dy: 2.25))
  expectEqual(negativeFractional, CGVector(dx: -1.25, dy: -2.25))

  // Initialize from integer literals.
  expectEqual(integral, CGVector(dx: 1, dy: 2))
  expectEqual(negativeIntegral, CGVector(dx: -1, dy: -2))

  expectEqual(fractional, CGVector(dx: 1.25 as CGFloat, dy: 2.25 as CGFloat))
  expectEqual(fractional, CGVector(dx: 1.25 as Double, dy: 2.25 as Double))
  expectEqual(integral, CGVector(dx: 1 as Int, dy: 2 as Int))
}

CoreGraphicsTests.test("CGVector.zero") {
  expectEqual(0.0, CGVector.zero.dx)
  expectEqual(0.0, CGVector.zero.dy)
}

runAllTests()

