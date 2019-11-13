// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPFillClearGenerateTests = TestSuite("Accelerate_vDSPFillClearGenerate")

//===----------------------------------------------------------------------===//
//
//  vDSP vector fill, clear, and generation tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
        
        vDSP.formWindow(usingSequence: .hanningNormalized,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_NORM))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hanningNormalized,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningNormalizedHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningNormalized,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_NORM | vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hanningNormalized,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningDenormalized") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningDenormalized,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_DENORM))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hanningDenormalized,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHanningDenormalizedHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningDenormalized,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hann_window(&legacyResult,
                         n,
                         Int32(vDSP_HANN_DENORM | vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hanningDenormalized,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHamming") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hamming,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hamm_window(&legacyResult,
                         n,
                         0)
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hamming,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionHammingHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hamming,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hamm_window(&legacyResult,
                         n,
                         Int32(vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .hamming,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionBlackman") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .blackman,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_blkman_window(&legacyResult,
                           n,
                           0)
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .blackman,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionBlackmanHalf") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .blackman,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_blkman_window(&legacyResult,
                           n,
                           Int32(vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Float.self,
                                         usingSequence: .blackman,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningNormalized") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningNormalized,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_NORM))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hanningNormalized,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningNormalizedHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningNormalized,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_NORM | vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hanningNormalized,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningDenormalized") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningDenormalized,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_DENORM))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hanningDenormalized,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHanningDenormalizedHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hanningDenormalized,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hann_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HANN_DENORM | vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hanningDenormalized,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHamming") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hamming,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_hamm_windowD(&legacyResult,
                          n,
                          0)
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hamming,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionHammingHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .hamming,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_hamm_windowD(&legacyResult,
                          n,
                          Int32(vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .hamming,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionBlackman") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .blackman,
                        result: &result,
                        isHalfWindow: false)
        
        vDSP_blkman_windowD(&legacyResult,
                            n,
                            0)
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .blackman,
                                         count: count,
                                         isHalfWindow: false)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionBlackmanHalf") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formWindow(usingSequence: .blackman,
                        result: &result,
                        isHalfWindow: true)
        
        vDSP_blkman_windowD(&legacyResult,
                            n,
                            Int32(vDSP_HALF_WINDOW))
        
        let returnedResult = vDSP.window(ofType: Double.self,
                                         usingSequence: .blackman,
                                         count: count,
                                         isHalfWindow: true)
        
        expectTrue(result[0 ..< count / 2].elementsEqual(legacyResult[0 ..< count / 2]))
        expectTrue(result[0 ..< count / 2].elementsEqual(returnedResult[0 ..< count / 2]))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionWithRange") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formRamp(in: 0 ... 100,
                      result: &result)
        
        vDSP_vgen([0],
                  [100],
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.ramp(in: Float(0) ... Float(100),
                                       count: count)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionRampWithIncrement") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.formRamp(withInitialValue: 0,
                      increment: 0.1,
                      result: &result)
        
        vDSP_vramp([0],
                   [0.1],
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.ramp(withInitialValue: Float(0),
                                       increment: Float(0.1),
                                       count: count)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/SinglePrecisionRampWithMultiplier") {
        var result = [Float](repeating: 1, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        let multiplier = (0 ..< n).map{ i in
            return sin(Float(i) * 0.05)
        }
        
        var start = Float(10)
        
        vDSP.formRamp(withInitialValue: &start,
                      multiplyingBy: multiplier,
                      increment: 1,
                      result: &result)
        
        var legacyStart = Float(10)
        
        vDSP_vrampmul(multiplier, 1,
                      &legacyStart,
                      [1],
                      &legacyResult, 1,
                      n)
        
        var start2 = Float(10)
        let returnedResult = vDSP.ramp(withInitialValue: &start2,
                                       multiplyingBy: multiplier,
                                       increment: 1)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
   
        expectEqual(start, legacyStart)
        expectEqual(start, start2)
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
        
        vDSP.formStereoRamp(withInitialValue: &start,
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
        
        var start2 = Float(10)
        let returnedResult = vDSP.stereoRamp(withInitialValue: &start2,
                                                multiplyingBy: multiplierOne, multiplierTwo,
                                                increment: 1)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(resultTwo.elementsEqual(legacyResultTwo))
        expectEqual(start, legacyStart)
        
        expectTrue(result.elementsEqual(returnedResult.firstOutput))
        expectTrue(resultTwo.elementsEqual(returnedResult.secondOutput))
        expectEqual(start, start2)
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithRange") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formRamp(in: 0 ... 100,
                      result: &result)
        
        vDSP_vgenD([0],
                   [100],
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.ramp(in: Double(0) ... Double(100),
                                       count: count)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithRange") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.formRamp(withInitialValue: 0,
                      increment: 0.1,
                      result: &result)
        
        vDSP_vrampD([0],
                    [0.1],
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.ramp(withInitialValue: 0,
                                       increment: 0.1,
                                       count: count)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPFillClearGenerateTests.test("vDSP/DoublePrecisionRampWithMultiplier") {
        var result = [Double](repeating: 1, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        let multiplier = (0 ..< n).map{ i in
            return sin(Double(i) * 0.05)
        }
        
        var start = Double(10)
        
        vDSP.formRamp(withInitialValue: &start,
                      multiplyingBy: multiplier,
                      increment: 1,
                      result: &result)
        
        var legacyStart = Double(10)
        
        vDSP_vrampmulD(multiplier, 1,
                       &legacyStart,
                       [1],
                       &legacyResult, 1,
                       n)

        var start2 = Double(10)
        let returnedResult = vDSP.ramp(withInitialValue: &start2,
                                       multiplyingBy: multiplier,
                                       increment: 1)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
        
        expectEqual(start, legacyStart)
        expectEqual(start, start2)
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
        
        vDSP.formStereoRamp(withInitialValue: &start,
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
        
        var start2 = Double(10)
        let returnedResult = vDSP.stereoRamp(withInitialValue: &start2,
                                                 multiplyingBy: multiplierOne, multiplierTwo,
                                                 increment: 1)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(resultTwo.elementsEqual(legacyResultTwo))
        expectEqual(start, legacyStart)
        
        expectTrue(result.elementsEqual(returnedResult.firstOutput))
        expectTrue(resultTwo.elementsEqual(returnedResult.secondOutput))
        expectEqual(start, start2)
    }
}

runAllTests()
