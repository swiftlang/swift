// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPFillClearGenerateTests = TestSuite("Accelerate_vDSPFillClearGenerate")

if #available(iOS 10.0, OSX 10.12, tvOS 10.0, watchOS 4.0, *) {
  
  Accelerate_vDSPFillClearGenerateTests.test("BNNS/ImageStackDescriptor") {
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
  
  Accelerate_vDSPFillClearGenerateTests.test("BNNS/VectorDescriptor") {
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
  
  Accelerate_vDSPFillClearGenerateTests.test("BNNS/LayerData") {
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
  
  Accelerate_vDSPFillClearGenerateTests.test("BNNS/Activation") {
    expectEqual(BNNSActivation.identity.function, .identity)
    let id = BNNSActivation(function: .identity)
    expectTrue(id.alpha.isNaN)
    expectTrue(id.beta.isNaN)
  }
  
}

//===----------------------------------------------------------------------===//
//
//  vDSP vector fill, clear, and generation tests
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionFill") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.fill(&result,
                        with: 999)
        
        vDSP_vfill([999],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionClear") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.clear(&result)
        
        vDSP_vclr(&legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionFill") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.fill(&result,
                        with: 999)
        
        vDSP_vfillD([999],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionClear") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.clear(&result)
        
        vDSP_vclrD(&legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningNormalized") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningNormalized,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_NORM))
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningNormalizedHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningNormalized,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_NORM | vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningDenormalized") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningDenormalized,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_DENORM))
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningDenormalizedHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningDenormalized,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_DENORM | vDSP_HALF_WINDOW))
        
         expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHamming") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hamming,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hamm_window(&legacyResult,
                         n,
                         0)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHammingHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hamming,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hamm_window(&legacyResult,
                         n,
                         Int32(vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionBlackman") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .blackman,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_blkman_window(&legacyResult,
                           n,
                           0)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionBlackmanHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .blackman,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_blkman_window(&legacyResult,
                           n,
                           Int32(vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningNormalized") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningNormalized,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_NORM))
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningNormalizedHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningNormalized,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_NORM | vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningDenormalized") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningDenormalized,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_DENORM))
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningDenormalizedHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hanningDenormalized,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_DENORM | vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHamming") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hamming,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_hamm_windowD(&legacyResult,
                          n,
                          0)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHammingHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .hamming,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_hamm_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionBlackman") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .blackman,
                                  result: &result,
                                  isHalfWindow: false)
        
        vDSP_blkman_windowD(&legacyResult,
                           n,
                           0)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionBlackmanHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateWindow(ofType: .blackman,
                                  result: &result,
                                  isHalfWindow: true)
        
        vDSP_blkman_windowD(&legacyResult,
                            n,
                            Int32(vDSP_HALF_WINDOW))
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionRampWithRange") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateRamp(in: 0 ... 100,
                                result: &result)
        
        vDSP_vgen([0],
                  [100],
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionRampWithIncrement") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.generateRamp(withInitialValue: 0,
                                increment: 0.1,
                                result: &result)
        
        vDSP_vramp([0],
                   [0.1],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionRampWithMultiplier") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        let multiplier = (0 ..< n).map{ i in
            return sin(Float(i) * 0.05)
        }
        
        var start = Float(10)
        
        vDSP.generateRamp(withInitialValue: &start,
                                multiplyingBy: multiplier,
                                increment: 1,
                                result: &result)
        
        var legacyStart = Float(10)
        
        vDSP_vrampmul(multiplier, 1,
                      &legacyStart,
                      [1],
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectEqual(start, legacyStart)
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/StereoSinglePrecisionRampWithMultiplier") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        let multiplierOne = (0 ..< n).map{ i in
            return sin(Float(i) * 0.05)
        }
        let multiplierTwo = (0 ..< n).map{ i in
            return sin(Float(i) * 0.0125)
        }
        
        var start = Float(10)
        var resultTwo = [Float](repeating: 2, count: result.count)
        
        vDSP.generateStereoRamp(withInitialValue: &start,
                                      multiplyingBy: multiplierOne, multiplierTwo,
                                      increment: 1,
                                      results: &result, &resultTwo)
        
        var legacyStart = Float(10)
        var legacyResultTwo = [Float](repeating: -2, count: legacyResult.count)
        
        vDSP_vrampmul2(multiplierOne, multiplierTwo, 1,
                       &legacyStart,
                       [1],
                       &legacyResult, &legacyResultTwo, 1,
                       n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(resultTwo.elementsEqual(legacyResultTwo))
        expectEqual(start, legacyStart)
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithRange") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateRamp(in: 0 ... 100,
                                result: &result)
        
        vDSP_vgenD([0],
                   [100],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithRange") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.generateRamp(withInitialValue: 0,
                                increment: 0.1,
                                result: &result)
        
        vDSP_vrampD([0],
                    [0.1],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithMultiplier") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        let multiplier = (0 ..< n).map{ i in
            return sin(Double(i) * 0.05)
        }
        
        var start = Double(10)
        
        vDSP.generateRamp(withInitialValue: &start,
                                multiplyingBy: multiplier,
                                increment: 1,
                                result: &result)
        
        var legacyStart = Double(10)
        
        vDSP_vrampmulD(multiplier, 1,
                       &legacyStart,
                       [1],
                       &legacyResult, 1,
                       n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectEqual(start, legacyStart)
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/StereoDoublePrecisionRampWithMultiplier") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        let multiplierOne = (0 ..< n).map{ i in
            return sin(Double(i) * 0.05)
        }
        let multiplierTwo = (0 ..< n).map{ i in
            return sin(Double(i) * 0.0125)
        }
        
        var start = Double(10)
        var resultTwo = [Double](repeating: 2, count: result.count)
        
        vDSP.generateStereoRamp(withInitialValue: &start,
                                multiplyingBy: multiplierOne, multiplierTwo,
                                increment: 1,
                                results: &result, &resultTwo)
        
        var legacyStart = Double(10)
        var legacyResultTwo = [Double](repeating: -2, count: legacyResult.count)
        
        vDSP_vrampmul2D(multiplierOne, multiplierTwo, 1,
                        &legacyStart,
                        [1],
                        &legacyResult, &legacyResultTwo, 1,
                        n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(resultTwo.elementsEqual(legacyResultTwo))
        expectEqual(start, legacyStart)
    }
}

runAllTests()
