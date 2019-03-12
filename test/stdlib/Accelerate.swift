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
//  vDSP clipping, limit, and threshold tests; single-precision.
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    let bounds = Float(-0.5) ... Float(0.5)
    let outputConstant: Float = 99
    
    let source: [Float] = (0 ..< 256).map { i in
        return sin(Float(i) * 0.05) + sin(Float(i) * 0.025)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionClipping") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.clip(source,
                  to: bounds,
                  result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vclip(source, 1,
                   [bounds.lowerBound],
                   [bounds.upperBound],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionInvertedClipping") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.invertedClip(source,
                          to: bounds,
                          result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_viclip(source, 1,
                    [bounds.lowerBound],
                    [bounds.upperBound],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionThreshold") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .clampToThreshold,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vthr(source, 1,
                  [bounds.lowerBound],
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionThresholdWithConstant") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .signedConstant(outputConstant),
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vthrsc(source, 1,
                    [bounds.lowerBound],
                    [outputConstant],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionThresholdWithZeroFill") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .zeroFill,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vthres(source, 1,
                    [bounds.lowerBound],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionLimit") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.limit(source,
                   limit: bounds.upperBound,
                   withOutputConstant: outputConstant,
                   result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vlim(source, 1,
                  [bounds.upperBound],
                  [outputConstant],
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP clipping, limit, and threshold tests; double-precision.
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    let bounds = Double(-0.5) ... Double(0.5)
    let outputConstant: Double = 99
    
    let source: [Double] = (0 ..< 256).map { i in
        return sin(Double(i) * 0.05) + sin(Double(i) * 0.025)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionClipping") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.clip(source,
                  to: bounds,
                  result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vclipD(source, 1,
                    [bounds.lowerBound],
                    [bounds.upperBound],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionInvertedClipping") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.invertedClip(source,
                          to: bounds,
                          result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_viclipD(source, 1,
                     [bounds.lowerBound],
                     [bounds.upperBound],
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionThreshold") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .clampToThreshold,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vthrD(source, 1,
                   [bounds.lowerBound],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionThresholdWithConstant") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .signedConstant(outputConstant),
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vthrscD(source, 1,
                     [bounds.lowerBound],
                     [outputConstant],
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionThresholdWithZeroFill") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.threshold(source,
                       to: bounds.lowerBound,
                       with: .zeroFill,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vthresD(source, 1,
                     [bounds.lowerBound],
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionLimit") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.limit(source,
                   limit: bounds.upperBound,
                   withOutputConstant: outputConstant,
                   result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vlimD(source, 1,
                   [bounds.upperBound],
                   [outputConstant],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

runAllTests()
