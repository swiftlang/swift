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
//  vDSP vector reduction; single-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = vDSP_Length(256)
    
    let a = (0 ..< 256).map {
        return sin(Float($0) * 0.03) * cos(Float($0) * 0.07)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMaximum") {
        let result: Float = vDSP.maximum(a)
        
        var legacyResult = Float(0)
        
        vDSP_maxv(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMaximumIndex") {
        let result: (vDSP_Length, Float) = vDSP.maximum(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxvi(a, 1,
                   &legacyValueResult,
                   &legacyIndexResult,
                   n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMaximumMagnitude") {
        let result: Float = vDSP.maximumMagnitude(a)
        
        var legacyResult = Float(0)
        
        vDSP_maxmgv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMaximumMagnitudeIndex") {
        let result: (vDSP_Length, Float) = vDSP.maximumMagnitude(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxmgvi(a, 1,
                     &legacyValueResult,
                     &legacyIndexResult,
                     n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMaximumMagnitudeIndex") {
        let result: Float = vDSP.minimum(a)
        
        var legacyResult = Float(0)
        
        vDSP_minv(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMinimumIndex") {
        let result: (vDSP_Length, Float) = vDSP.minimum(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_minvi(a, 1,
                   &legacyValueResult,
                   &legacyIndexResult,
                   n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionSum") {
        let result: Float = vDSP.sum(a)
        
        var legacyResult = Float(0)
        
        vDSP_sve(a, 1,
                 &legacyResult,
                 n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionSumOfSquares") {
        let result: Float = vDSP.sumOfSquares(a)
        
        var legacyResult = Float(0)
        
        vDSP_svesq(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionSumOfMagnitudes") {
        let result: Float = vDSP.sumOfMagnitudes(a)
        
        var legacyResult = Float(0)
        
        vDSP_svemg(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.meanSquare(a)
        
        var legacyResult = Float.nan
        
        vDSP_measqv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.meanMagnitude(a)
        
        var legacyResult = Float.nan
        
        vDSP_meamgv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.mean(a)
        
        var legacyResult = Float.nan
        
        vDSP_meanv(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/RMSSinglePrecision") {
        let result = vDSP.rootMeanSquare(a)
        
        var legacyResult = Float.nan
        
        vDSP_rmsqv(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP vector reduction; double-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = vDSP_Length(256)
    
    let a = (0 ..< 256).map {
        return sin(Double($0) * 0.03) * cos(Double($0) * 0.07)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMaximum") {
        let result: Double = vDSP.maximum(a)
        
        var legacyResult = Double(0)
        
        vDSP_maxvD(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMaximumIndex") {
        let result: (vDSP_Length, Double) = vDSP.maximum(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxviD(a, 1,
                    &legacyValueResult,
                    &legacyIndexResult,
                    n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMaximumMagnitude") {
        let result: Double = vDSP.maximumMagnitude(a)
        
        var legacyResult = Double(0)
        
        vDSP_maxmgvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMaximumMagnitudeIndex") {
        let result: (vDSP_Length, Double) = vDSP.maximumMagnitude(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxmgviD(a, 1,
                      &legacyValueResult,
                      &legacyIndexResult,
                      n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMaximumMagnitudeIndex") {
        let result: Double = vDSP.minimum(a)
        
        var legacyResult = Double(0)
        
        vDSP_minvD(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMinimumIndex") {
        let result: (vDSP_Length, Double) = vDSP.minimum(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_minviD(a, 1,
                    &legacyValueResult,
                    &legacyIndexResult,
                    n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionSum") {
        let result: Double = vDSP.sum(a)
        
        var legacyResult = Double(0)
        
        vDSP_sveD(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionSumOfSquares") {
        let result: Double = vDSP.sumOfSquares(a)
        
        var legacyResult = Double(0)
        
        vDSP_svesqD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionSumOfMagnitudes") {
        let result: Double = vDSP.sumOfMagnitudes(a)
        
        var legacyResult = Double(0)
        
        vDSP_svemgD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.meanSquare(a)
        
        var legacyResult = Double.nan
        
        vDSP_measqvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.meanMagnitude(a)
        
        var legacyResult = Double.nan
        
        vDSP_meamgvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.mean(a)
        
        var legacyResult = Double.nan
        
        vDSP_meanvD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    AccelerateTests.test("vDSP/RMSDoublePrecision") {
        let result = vDSP.rootMeanSquare(a)
        
        var legacyResult = Double.nan
        
        vDSP_rmsqvD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
}

runAllTests()
