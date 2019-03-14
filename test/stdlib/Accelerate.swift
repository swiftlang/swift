// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var AccelerateTests = TestSuite("Accelerate")

if #available(iOS 10.0, OSX 10.12, tvOS 10.0, watchOS 4.0, *) {
  
  AccelerateTests.test("BNNS/ImageStackDescriptor") {
    var succeed = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                           row_stride: 0, image_stride: 0,
                                           data_type: .int8)
    expectEqual(succeed.data_scale, 1)
    expectEqual(succeed.data_bias, 0)
    succeed = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                       row_stride: 0, image_stride: 0,
                                       data_type: .int16,
                                       data_scale: 0.5, data_bias: 0.5)
    expectEqual(succeed.data_scale, 0.5)
    expectEqual(succeed.data_bias, 0.5)
    expectCrashLater()
    //  indexed8 is not allowed as an imageStack data type.
    let _ = BNNSImageStackDescriptor(width: 0, height: 0, channels: 0,
                                     row_stride: 0, image_stride: 0,
                                     data_type: .indexed8)
  }
  
  AccelerateTests.test("BNNS/VectorDescriptor") {
    var succeed = BNNSVectorDescriptor(size: 0, data_type: .int8)
    expectEqual(succeed.data_scale, 1)
    expectEqual(succeed.data_bias, 0)
    succeed = BNNSVectorDescriptor(size: 0, data_type: .int8,
                                   data_scale: 0.5, data_bias: 0.5)
    expectEqual(succeed.data_scale, 0.5)
    expectEqual(succeed.data_bias, 0.5)
    expectCrashLater()
    //  indexed8 is not allowed as a vector data type.
    let _ = BNNSVectorDescriptor(size: 0, data_type: .indexed8)
  }
  
  AccelerateTests.test("BNNS/LayerData") {
    //  The zero layer should have data == nil.
    expectEqual(BNNSLayerData.zero.data, nil)
    var succeed = BNNSLayerData(data: nil, data_type: .int8)
    expectEqual(succeed.data_scale, 1)
    expectEqual(succeed.data_bias, 0)
    succeed = BNNSLayerData(data: nil, data_type: .int8, data_scale: 0.5,
                            data_bias: 0.5, data_table: nil)
    expectEqual(succeed.data_scale, 0.5)
    expectEqual(succeed.data_bias, 0.5)
    var table: [Float] = [1.0]
    succeed = BNNSLayerData.indexed8(data: nil, data_table: &table)
    expectCrashLater()
    // indexed8 requires a non-nil data table.
    let _ = BNNSLayerData(data: nil, data_type: .indexed8)
  }
  
  AccelerateTests.test("BNNS/Activation") {
    expectEqual(BNNSActivation.identity.function, .identity)
    let id = BNNSActivation(function: .identity)
    expectTrue(id.alpha.isNaN)
    expectTrue(id.beta.isNaN)
  }
  
}

//===----------------------------------------------------------------------===//
//
//  vDSP Dot Product
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("vDSP/SinglePrecisionDot") {
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
    
    AccelerateTests.test("vDSP/DoublePrecisionDot") {
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

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let xlegs = [(1, 2),        (4, 5),     (2, 1),         (1000, 1), (-1055, 55)]
    let ylegs = [(-1000, 2),    (4, -50),   (-25, -100),    (1, 1000), (5, 1)]
    
    let pointA = [1,    4,      1,  -100,   -10, 10,    99]
    let pointB = [901,  14,     61, -1,     -10, -1000, 27]
    
    AccelerateTests.test("vDSP/SinglePrecisionDist") {
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
        
        expectTrue(distResult.elementsEqual(legacyDistResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionPyth") {
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
        
        expectTrue(pythgResult.elementsEqual(legacyPythgResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionDist") {
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
        
        expectTrue(distResult.elementsEqual(legacyDistResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionPyth") {
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
        
        expectTrue(pythgResult.elementsEqual(legacyPythgResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionDistanceSquared") {
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
    
    AccelerateTests.test("vDSP/DoublePrecisionDistanceSquared") {
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
