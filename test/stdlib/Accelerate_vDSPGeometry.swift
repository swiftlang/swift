// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPGeometryTests = TestSuite("Accelerate_vDSPGeometry")

//===----------------------------------------------------------------------===//
//
//  vDSP Dot Product
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    Accelerate_vDSPGeometryTests.test("vDSP/SinglePrecisionDot") {
        let a: [Float] = [ 1.2, 6.7, 0.22334, 101.9, 90.1, 100.999 ]
        let b: [Float] = [99.9, 0.1, 1000.88, 23.99, 27.9, 87.0444]
        
        let result = vDSP.dot(a, b)
        
        var legacyResult = Float.nan
        
        vDSP_dotpr(a, 1,
                   b, 1,
                   &legacyResult,
                   vDSP_Length(a.count))
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/DoublePrecisionDot") {
        let a: [Double] = [ 1.2, 6.7, 0.22334, 101.9, 90.1, 100.999 ]
        let b: [Double] = [99.9, 0.1, 1000.88, 23.99, 27.9, 87.0444]
        
        let result = vDSP.dot(a, b)
        
        var legacyResult = Double.nan
        
        vDSP_dotprD(a, 1,
                    b, 1,
                    &legacyResult,
                    vDSP_Length(a.count))
        
        expectEqual(result, legacyResult)
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Hypotenuse and distance squared
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let xlegs = [(1, 2),        (4, 5),     (2, 1),         (1000, 1), (-1055, 55)]
    let ylegs = [(-1000, 2),    (4, -50),   (-25, -100),    (1, 1000), (5, 1)]
    
    let pointA = [1,    4,      1,  -100,   -10, 10,    99]
    let pointB = [901,  14,     61, -1,     -10, -1000, 27]
    
    Accelerate_vDSPGeometryTests.test("vDSP/SinglePrecisionDist") {
        var distResult = [Float](repeating: 0, count: xlegs.count)
        
        let x: [Float] = xlegs.map{ return Float($0.0) - Float($0.1) }
        let y: [Float] = ylegs.map{ return Float($0.0) - Float($0.1) }
        
        vDSP.hypot(x, y,
                   result: &distResult)
        
        var legacyDistResult = [Float](repeating: -1, count: xlegs.count)
        
        vDSP_vdist(x, 1,
                   y, 1,
                   &legacyDistResult, 1,
                   vDSP_Length(legacyDistResult.count))
        
        let returnedResult = vDSP.hypot(x, y)
        
        expectTrue(distResult.elementsEqual(legacyDistResult))
        expectTrue(distResult.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/SinglePrecisionPyth") {
        var pythgResult =  [Float](repeating: 0, count: xlegs.count)
        
        let x0: [Float] = xlegs.map{ return Float($0.0) }
        let x1: [Float] = xlegs.map{ return Float($0.1) }
        let y0: [Float] = ylegs.map{ return Float($0.0) }
        let y1: [Float] = ylegs.map{ return Float($0.1) }
        
        vDSP.hypot(x0: x0, x1: x1,
                   y0: y0, y1: y1,
                   result: &pythgResult)
        
        var legacyPythgResult = [Float](repeating: -1, count: xlegs.count)
        
        vDSP_vpythg(x0, 1, y0, 1,
                    x1, 1, y1, 1,
                    &legacyPythgResult, 1,
                    vDSP_Length(legacyPythgResult.count))
        
        let returnedResult = vDSP.hypot(x0: x0, x1: x1,
                                        y0: y0, y1: y1)
        
        expectTrue(pythgResult.elementsEqual(legacyPythgResult))
        expectTrue(pythgResult.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/DoublePrecisionDist") {
        var distResult = [Double](repeating: 0, count: xlegs.count)
        
        let x: [Double] = xlegs.map{ return Double($0.0) - Double($0.1) }
        let y: [Double] = ylegs.map{ return Double($0.0) - Double($0.1) }
        
        vDSP.hypot(x, y,
                   result: &distResult)
        
        var legacyDistResult = [Double](repeating: -1, count: xlegs.count)
        
        vDSP_vdistD(x, 1,
                    y, 1,
                    &legacyDistResult, 1,
                    vDSP_Length(legacyDistResult.count))
        
        let returnedResult = vDSP.hypot(x, y)
        
        expectTrue(distResult.elementsEqual(legacyDistResult))
        expectTrue(distResult.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/DoublePrecisionPyth") {
        var pythgResult =  [Double](repeating: 0, count: xlegs.count)
        
        let x0: [Double] = xlegs.map{ return Double($0.0) }
        let x1: [Double] = xlegs.map{ return Double($0.1) }
        let y0: [Double] = ylegs.map{ return Double($0.0) }
        let y1: [Double] = ylegs.map{ return Double($0.1) }
        
        vDSP.hypot(x0: x0, x1: x1,
                   y0: y0, y1: y1,
                   result: &pythgResult)
        
        var legacyPythgResult = [Double](repeating: -1, count: xlegs.count)
        
        vDSP_vpythgD(x0, 1, y0, 1,
                     x1, 1, y1, 1,
                     &legacyPythgResult, 1,
                     vDSP_Length(legacyPythgResult.count))
        
        let returnedResult = vDSP.hypot(x0: x0, x1: x1,
                                        y0: y0, y1: y1)
        
        expectTrue(pythgResult.elementsEqual(legacyPythgResult))
        expectTrue(pythgResult.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/SinglePrecisionDistanceSquared") {
        let a = pointA.map{ return Float($0) }
        let b = pointB.map{ return Float($0) }
        
        let result = vDSP.distanceSquared(a, b)
        
        var legacyResult = Float.nan
        
        vDSP_distancesq(a, 1,
                        b, 1,
                        &legacyResult,
                        vDSP_Length(a.count))
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPGeometryTests.test("vDSP/DoublePrecisionDistanceSquared") {
        let a = pointA.map{ return Double($0) }
        let b = pointB.map{ return Double($0) }
        
        let result = vDSP.distanceSquared(a, b)
        
        var legacyResult = Double.nan
        
        vDSP_distancesqD(a, 1,
                         b, 1,
                         &legacyResult,
                         vDSP_Length(a.count))
        
        expectEqual(result, legacyResult)
    }
}

runAllTests()
