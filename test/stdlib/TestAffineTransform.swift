// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

#if os(macOS)

#if FOUNDATION_XCTEST
import XCTest
class TestAffineTransformSuper : XCTestCase { }
#else
import StdlibUnittest
class TestAffineTransformSuper { }
#endif

func expectEqualWithAccuracy(_ lhs: Double, _ rhs: Double, accuracy: Double, _ message: String = "", file: String = #file, line: UInt = #line) {
    expectTrue(fabs(lhs - rhs) < accuracy, message, file: file, line: line)
}

extension AffineTransform {
    func transform(_ aRect: NSRect) -> NSRect {
        return NSRect(origin: transform(aRect.origin), size: transform(aRect.size))
    }
}

class TestAffineTransform : TestAffineTransformSuper {
    private let accuracyThreshold = 0.001
    
    func checkPointTransformation(_ transform: AffineTransform, point: NSPoint, expectedPoint: NSPoint, _ message: String = "", file: String = #file, line: UInt = #line) {
        let newPoint = transform.transform(point)
        expectEqualWithAccuracy(Double(newPoint.x), Double(expectedPoint.x), accuracy: accuracyThreshold,
                                   "x (expected: \(expectedPoint.x), was: \(newPoint.x)): \(message)", file: file, line: line)
        expectEqualWithAccuracy(Double(newPoint.y), Double(expectedPoint.y), accuracy: accuracyThreshold,
                                   "y (expected: \(expectedPoint.y), was: \(newPoint.y)): \(message)", file: file, line: line)
    }
    
    func checkSizeTransformation(_ transform: AffineTransform, size: NSSize, expectedSize: NSSize, _ message: String = "", file: String = #file, line: UInt = #line) {
        let newSize = transform.transform(size)
        expectEqualWithAccuracy(Double(newSize.width), Double(expectedSize.width), accuracy: accuracyThreshold,
                                   "width (expected: \(expectedSize.width), was: \(newSize.width)): \(message)", file: file, line: line)
        expectEqualWithAccuracy(Double(newSize.height), Double(expectedSize.height), accuracy: accuracyThreshold,
                                   "height (expected: \(expectedSize.height), was: \(newSize.height)): \(message)", file: file, line: line)
    }
    
    func checkRectTransformation(_ transform: AffineTransform, rect: NSRect, expectedRect: NSRect, _ message: String = "", file: String = #file, line: UInt = #line) {
        let newRect = transform.transform(rect)
        
        checkPointTransformation(transform, point: newRect.origin, expectedPoint: expectedRect.origin,
                                 "origin (expected: \(expectedRect.origin), was: \(newRect.origin)): \(message)", file: file, line: line)
        checkSizeTransformation(transform, size: newRect.size, expectedSize: expectedRect.size,
                                "size (expected: \(expectedRect.size), was: \(newRect.size)): \(message)", file: file, line: line)
    }
    
    func test_BasicConstruction() {
        let defaultAffineTransform = AffineTransform()
        let identityTransform = AffineTransform.identity

        expectEqual(defaultAffineTransform, identityTransform)
        
        // The diagonal entries (1,1) and (2,2) of the identity matrix are ones. The other entries are zeros.
        // TODO: These should use DBL_MAX but it's not available as part of Glibc on Linux
        expectEqualWithAccuracy(Double(identityTransform.m11), Double(1), accuracy: accuracyThreshold)
        expectEqualWithAccuracy(Double(identityTransform.m22), Double(1), accuracy: accuracyThreshold)
        
        expectEqualWithAccuracy(Double(identityTransform.m12), Double(0), accuracy: accuracyThreshold)
        expectEqualWithAccuracy(Double(identityTransform.m21), Double(0), accuracy: accuracyThreshold)
        expectEqualWithAccuracy(Double(identityTransform.tX), Double(0), accuracy: accuracyThreshold)
        expectEqualWithAccuracy(Double(identityTransform.tY), Double(0), accuracy: accuracyThreshold)
    }
    
    func test_IdentityTransformation() {
        let identityTransform = AffineTransform.identity
        
        func checkIdentityPointTransformation(_ point: NSPoint) {
            checkPointTransformation(identityTransform, point: point, expectedPoint: point)
        }
        
        checkIdentityPointTransformation(NSPoint())
        checkIdentityPointTransformation(NSPoint(x: CGFloat(24.5), y: CGFloat(10.0)))
        checkIdentityPointTransformation(NSPoint(x: CGFloat(-7.5), y: CGFloat(2.0)))
        
        func checkIdentitySizeTransformation(_ size: NSSize) {
            checkSizeTransformation(identityTransform, size: size, expectedSize: size)
        }
        
        checkIdentitySizeTransformation(NSSize())
        checkIdentitySizeTransformation(NSSize(width: CGFloat(13.0), height: CGFloat(12.5)))
        checkIdentitySizeTransformation(NSSize(width: CGFloat(100.0), height: CGFloat(-100.0)))
    }
    
    func test_Translation() {
        let point = NSPoint(x: CGFloat(0.0), y: CGFloat(0.0))
        
        var noop = AffineTransform.identity
        noop.translate(x: CGFloat(), y: CGFloat())
        checkPointTransformation(noop, point: point, expectedPoint: point)
        
        var translateH = AffineTransform.identity
        translateH.translate(x: CGFloat(10.0), y: CGFloat())
        checkPointTransformation(translateH, point: point, expectedPoint: NSPoint(x: CGFloat(10.0), y: CGFloat()))
        
        var translateV = AffineTransform.identity
        translateV.translate(x: CGFloat(), y: CGFloat(20.0))
        checkPointTransformation(translateV, point: point, expectedPoint: NSPoint(x: CGFloat(), y: CGFloat(20.0)))
        
        var translate = AffineTransform.identity
        translate.translate(x: CGFloat(-30.0), y: CGFloat(40.0))
        checkPointTransformation(translate, point: point, expectedPoint: NSPoint(x: CGFloat(-30.0), y: CGFloat(40.0)))
    }
    
    func test_Scale() {
        let size = NSSize(width: CGFloat(10.0), height: CGFloat(10.0))
        
        var noop = AffineTransform.identity
        noop.scale(CGFloat(1.0))
        checkSizeTransformation(noop, size: size, expectedSize: size)
        
        var shrink = AffineTransform.identity
        shrink.scale(CGFloat(0.5))
        checkSizeTransformation(shrink, size: size, expectedSize: NSSize(width: CGFloat(5.0), height: CGFloat(5.0)))
        
        var grow = AffineTransform.identity
        grow.scale(CGFloat(3.0))
        checkSizeTransformation(grow, size: size, expectedSize: NSSize(width: CGFloat(30.0), height: CGFloat(30.0)))
        
        var stretch = AffineTransform.identity
        stretch.scale(x: CGFloat(2.0), y: CGFloat(0.5))
        checkSizeTransformation(stretch, size: size, expectedSize: NSSize(width: CGFloat(20.0), height: CGFloat(5.0)))
    }
    
    func test_Rotation_Degrees() {
        let point = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        
        var noop = AffineTransform.identity
        noop.rotate(byDegrees: CGFloat())
        checkPointTransformation(noop, point: point, expectedPoint: point)
        
        var tenEighty = AffineTransform.identity
        tenEighty.rotate(byDegrees: CGFloat(1080.0))
        checkPointTransformation(tenEighty, point: point, expectedPoint: point)
        
        var rotateCounterClockwise = AffineTransform.identity
        rotateCounterClockwise.rotate(byDegrees: CGFloat(90.0))
        checkPointTransformation(rotateCounterClockwise, point: point, expectedPoint: NSPoint(x: CGFloat(-10.0), y: CGFloat(10.0)))
        
        var rotateClockwise = AffineTransform.identity
        rotateClockwise.rotate(byDegrees: CGFloat(-90.0))
        checkPointTransformation(rotateClockwise, point: point, expectedPoint: NSPoint(x: CGFloat(10.0), y: CGFloat(-10.0)))
        
        var reflectAboutOrigin = AffineTransform.identity
        reflectAboutOrigin.rotate(byDegrees: CGFloat(180.0))
        checkPointTransformation(reflectAboutOrigin, point: point, expectedPoint: NSPoint(x: CGFloat(-10.0), y: CGFloat(-10.0)))
    }
    
    func test_Rotation_Radians() {
        let point = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        
        var noop = AffineTransform.identity
        noop.rotate(byRadians: CGFloat())
        checkPointTransformation(noop, point: point, expectedPoint: point)
        
        var tenEighty = AffineTransform.identity
        tenEighty.rotate(byRadians: 6 * .pi)
        checkPointTransformation(tenEighty, point: point, expectedPoint: point)
        
        var rotateCounterClockwise = AffineTransform.identity
        rotateCounterClockwise.rotate(byRadians: .pi / 2)
        checkPointTransformation(rotateCounterClockwise, point: point, expectedPoint: NSPoint(x: CGFloat(-10.0), y: CGFloat(10.0)))
        
        var rotateClockwise = AffineTransform.identity
        rotateClockwise.rotate(byRadians: -.pi / 2)
        checkPointTransformation(rotateClockwise, point: point, expectedPoint: NSPoint(x: CGFloat(10.0), y: CGFloat(-10.0)))
        
        var reflectAboutOrigin = AffineTransform.identity
        reflectAboutOrigin.rotate(byRadians: .pi)
        checkPointTransformation(reflectAboutOrigin, point: point, expectedPoint: NSPoint(x: CGFloat(-10.0), y: CGFloat(-10.0)))
        
        var scaleThenRotate = AffineTransform(scale: 2)
        scaleThenRotate.rotate(byRadians: .pi / 2)
        checkPointTransformation(scaleThenRotate, point: point, expectedPoint: NSPoint(x: CGFloat(-20.0), y: CGFloat(20.0)))
    }
    
    func test_Inversion() {
        let point = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        
        var translate = AffineTransform.identity
        translate.translate(x: CGFloat(-30.0), y: CGFloat(40.0))
        
        var rotate = AffineTransform.identity
        translate.rotate(byDegrees: CGFloat(30.0))
        
        var scale = AffineTransform.identity
        scale.scale(CGFloat(2.0))
        
        var identityTransform = AffineTransform.identity
        
        // append transformations
        identityTransform.append(translate)
        identityTransform.append(rotate)
        identityTransform.append(scale)
        
        // invert transformations
        scale.invert()
        rotate.invert()
        translate.invert()
        
        // append inverse transformations in reverse order
        identityTransform.append(scale)
        identityTransform.append(rotate)
        identityTransform.append(translate)
        
        checkPointTransformation(identityTransform, point: point, expectedPoint: point)
    }
    
    func test_TranslationComposed() {
        var xyPlus5 = AffineTransform.identity
        xyPlus5.translate(x: CGFloat(2.0), y: CGFloat(3.0))
        xyPlus5.translate(x: CGFloat(3.0), y: CGFloat(2.0))
        
        checkPointTransformation(xyPlus5, point: NSPoint(x: CGFloat(-2.0), y: CGFloat(-3.0)),
                                 expectedPoint: NSPoint(x: CGFloat(3.0), y: CGFloat(2.0)))
    }
    
    func test_Scaling() {
        var xyTimes5 = AffineTransform.identity
        xyTimes5.scale(CGFloat(5.0))
        
        checkPointTransformation(xyTimes5, point: NSPoint(x: CGFloat(-2.0), y: CGFloat(3.0)),
                                 expectedPoint: NSPoint(x: CGFloat(-10.0), y: CGFloat(15.0)))
        
        var xTimes2YTimes3 = AffineTransform.identity
        xTimes2YTimes3.scale(x: CGFloat(2.0), y: CGFloat(-3.0))
        
        checkPointTransformation(xTimes2YTimes3, point: NSPoint(x: CGFloat(-1.0), y: CGFloat(3.5)),
                                 expectedPoint: NSPoint(x: CGFloat(-2.0), y: CGFloat(-10.5)))
    }
    
    func test_TranslationScaling() {
        var xPlus2XYTimes5 = AffineTransform.identity
        xPlus2XYTimes5.translate(x: CGFloat(2.0), y: CGFloat())
        xPlus2XYTimes5.scale(x: CGFloat(5.0), y: CGFloat(-5.0))
        
        checkPointTransformation(xPlus2XYTimes5, point: NSPoint(x: CGFloat(1.0), y: CGFloat(2.0)),
                                 expectedPoint: NSPoint(x: CGFloat(7.0), y: CGFloat(-10.0)))
    }
    
    func test_ScalingTranslation() {
        var xyTimes5XPlus3 = AffineTransform.identity
        xyTimes5XPlus3.scale(CGFloat(5.0))
        xyTimes5XPlus3.translate(x: CGFloat(3.0), y: CGFloat())
        
        checkPointTransformation(xyTimes5XPlus3, point: NSPoint(x: CGFloat(1.0), y: CGFloat(2.0)),
                                 expectedPoint: NSPoint(x: CGFloat(20.0), y: CGFloat(10.0)))
    }
    
    func test_AppendTransform() {
        let point = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        
        var identityTransform = AffineTransform.identity
        identityTransform.append(identityTransform)
        checkPointTransformation(identityTransform, point: point, expectedPoint: point)
        
        let translate = AffineTransform(translationByX: 10.0, byY: 0.0)
        
        let scale = AffineTransform(scale: 2.0)
        
        var translateThenScale = translate
        translateThenScale.append(scale)
        checkPointTransformation(translateThenScale, point: point, expectedPoint: NSPoint(x: CGFloat(40.0), y: CGFloat(20.0)))
    }
    
    func test_PrependTransform() {
        let point = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        
        var identityTransform = AffineTransform.identity
        identityTransform.append(identityTransform)
        checkPointTransformation(identityTransform, point: point, expectedPoint: point)
        
        let translate = AffineTransform(translationByX: 10.0, byY: 0.0)
        
        let scale = AffineTransform(scale: 2.0)
        
        var scaleThenTranslate = translate
        scaleThenTranslate.prepend(scale)
        checkPointTransformation(scaleThenTranslate, point: point, expectedPoint: NSPoint(x: CGFloat(30.0), y: CGFloat(20.0)))
    }
    
    func test_TransformComposition() {
        let origin = NSPoint(x: CGFloat(10.0), y: CGFloat(10.0))
        let size = NSSize(width: CGFloat(40.0), height: CGFloat(20.0))
        let rect = NSRect(origin: origin, size: size)
        let center = NSPoint(x: NSMidX(rect), y: NSMidY(rect))
        
        let rotate = AffineTransform(rotationByDegrees: 90.0)
        
        let moveOrigin = AffineTransform(translationByX: -center.x, byY: -center.y)
        
        var moveBack = moveOrigin
        moveBack.invert()
        
        var rotateAboutCenter = rotate
        rotateAboutCenter.prepend(moveOrigin)
        rotateAboutCenter.append(moveBack)
        
        // center of rect shouldn't move as its the rotation anchor
        checkPointTransformation(rotateAboutCenter, point: center, expectedPoint: center)
    }
    
    func test_hashing() {
        guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }

        // the transforms are made up and the values don't matter
        let a = AffineTransform(m11: 1.0, m12: 2.5, m21: 66.2, m22: 40.2, tX: -5.5, tY: 3.7)
        let b = AffineTransform(m11: -55.66, m12: 22.7, m21: 1.5, m22: 0.0, tX: -22, tY: -33)
        let c = AffineTransform(m11: 4.5, m12: 1.1, m21: 0.025, m22: 0.077, tX: -0.55, tY: 33.2)
        let d = AffineTransform(m11: 7.0, m12: -2.3, m21: 6.7, m22: 0.25, tX: 0.556, tY: 0.99)
        let e = AffineTransform(m11: 0.498, m12: -0.284, m21: -0.742, m22: 0.3248, tX: 12, tY: 44)

        // Samples testing that every component is properly hashed
        let x1 = AffineTransform(m11: 1.0, m12: 2.0, m21: 3.0, m22: 4.0, tX: 5.0, tY: 6.0)
        let x2 = AffineTransform(m11: 1.5, m12: 2.0, m21: 3.0, m22: 4.0, tX: 5.0, tY: 6.0)
        let x3 = AffineTransform(m11: 1.0, m12: 2.5, m21: 3.0, m22: 4.0, tX: 5.0, tY: 6.0)
        let x4 = AffineTransform(m11: 1.0, m12: 2.0, m21: 3.5, m22: 4.0, tX: 5.0, tY: 6.0)
        let x5 = AffineTransform(m11: 1.0, m12: 2.0, m21: 3.0, m22: 4.5, tX: 5.0, tY: 6.0)
        let x6 = AffineTransform(m11: 1.0, m12: 2.0, m21: 3.0, m22: 4.0, tX: 5.5, tY: 6.0)
        let x7 = AffineTransform(m11: 1.0, m12: 2.0, m21: 3.0, m22: 4.0, tX: 5.0, tY: 6.5)

        @inline(never)
        func bridged(_ t: AffineTransform) -> NSAffineTransform {
            return t as NSAffineTransform
        }

        let values: [[AffineTransform]] = [
            [AffineTransform.identity, NSAffineTransform() as AffineTransform],
            [a, bridged(a) as AffineTransform],
            [b, bridged(b) as AffineTransform],
            [c, bridged(c) as AffineTransform],
            [d, bridged(d) as AffineTransform],
            [e, bridged(e) as AffineTransform],
            [x1], [x2], [x3], [x4], [x5], [x6], [x7]
        ]
        checkHashableGroups(values)
    }

    func test_AnyHashable() {
        func makeNSAffineTransform(rotatedByDegrees angle: CGFloat) -> NSAffineTransform {
            let result = NSAffineTransform()
            result.rotate(byDegrees: angle)
            return result
        }

        let s1 = AffineTransform.identity
        let s2 = AffineTransform(m11: -55.66, m12: 22.7, m21: 1.5, m22: 0.0, tX: -22, tY: -33)
        let s3 = AffineTransform(m11: -55.66, m12: 22.7, m21: 1.5, m22: 0.0, tX: -22, tY: -33)
        let s4 = makeNSAffineTransform(rotatedByDegrees: 10) as AffineTransform
        let s5 = makeNSAffineTransform(rotatedByDegrees: 10) as AffineTransform

        let c1 = NSAffineTransform(transform: s1)
        let c2 = NSAffineTransform(transform: s2)
        let c3 = NSAffineTransform(transform: s3)
        let c4 = makeNSAffineTransform(rotatedByDegrees: 10)
        let c5 = makeNSAffineTransform(rotatedByDegrees: 10)

        let groups: [[AnyHashable]] = [
            [s1, c1],
            [s2, c2, s3, c3],
            [s4, c4, s5, c5]
        ]
        checkHashableGroups(groups)

        expectEqual(AffineTransform.self, type(of: (s1 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (s2 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (s3 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (s4 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (s5 as AnyHashable).base))

        expectEqual(AffineTransform.self, type(of: (c1 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (c2 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (c3 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (c4 as AnyHashable).base))
        expectEqual(AffineTransform.self, type(of: (c5 as AnyHashable).base))
    }

    func test_unconditionallyBridgeFromObjectiveC() {
        expectEqual(AffineTransform(), AffineTransform._unconditionallyBridgeFromObjectiveC(nil))
    }

    func test_rotation_compose() {
        var t = AffineTransform.identity
        t.translate(x: 1.0, y: 1.0)
        t.rotate(byDegrees: 90)
        t.translate(x: -1.0, y: -1.0)
        let result = t.transform(NSPoint(x: 1.0, y: 2.0))
        expectEqualWithAccuracy(0.0, Double(result.x), accuracy: accuracyThreshold)
        expectEqualWithAccuracy(1.0, Double(result.y), accuracy: accuracyThreshold)
    }
}

#if !FOUNDATION_XCTEST
var AffineTransformTests = TestSuite("TestAffineTransform")
AffineTransformTests.test("test_BasicConstruction") { TestAffineTransform().test_BasicConstruction() }
AffineTransformTests.test("test_IdentityTransformation") { TestAffineTransform().test_IdentityTransformation() }
AffineTransformTests.test("test_Translation") { TestAffineTransform().test_Translation() }
AffineTransformTests.test("test_Scale") { TestAffineTransform().test_Scale() }
AffineTransformTests.test("test_Rotation_Degrees") { TestAffineTransform().test_Rotation_Degrees() }
AffineTransformTests.test("test_Rotation_Radians") { TestAffineTransform().test_Rotation_Radians() }
AffineTransformTests.test("test_Inversion") { TestAffineTransform().test_Inversion() }
AffineTransformTests.test("test_TranslationComposed") { TestAffineTransform().test_TranslationComposed() }
AffineTransformTests.test("test_Scaling") { TestAffineTransform().test_Scaling() }
AffineTransformTests.test("test_TranslationScaling") { TestAffineTransform().test_TranslationScaling() }
AffineTransformTests.test("test_ScalingTranslation") { TestAffineTransform().test_ScalingTranslation() }
AffineTransformTests.test("test_AppendTransform") { TestAffineTransform().test_AppendTransform() }
AffineTransformTests.test("test_PrependTransform") { TestAffineTransform().test_PrependTransform() }
AffineTransformTests.test("test_TransformComposition") { TestAffineTransform().test_TransformComposition() }
AffineTransformTests.test("test_hashing") { TestAffineTransform().test_hashing() }
AffineTransformTests.test("test_AnyHashable") { TestAffineTransform().test_AnyHashable() }
AffineTransformTests.test("test_unconditionallyBridgeFromObjectiveC") { TestAffineTransform().test_unconditionallyBridgeFromObjectiveC() }
AffineTransformTests.test("test_rotation_compose") { TestAffineTransform().test_rotation_compose() }
runAllTests()
#endif
    

#endif
