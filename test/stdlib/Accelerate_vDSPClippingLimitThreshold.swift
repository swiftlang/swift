// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPClippingLimitThresholdTests = TestSuite("Accelerate_vDSPClippingLimitThreshold")

//===----------------------------------------------------------------------===//
//
//  vDSP clipping, limit, and threshold tests; single-precision.
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    let bounds = Float(-0.5) ... Float(0.5)
    let outputConstant: Float = 99
    
    let source: [Float] = (0 ..< 256).map { i in
        return sin(Float(i) * 0.05) + sin(Float(i) * 0.025)
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionClipping") {
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
        
        let returnedResult = vDSP.clip(source,
                                       to: bounds)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionInvertedClipping") {
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
        
        let returnedResult = vDSP.invertedClip(source,
                                               to: bounds)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionThreshold") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .clampToThreshold)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionThresholdWithConstant") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .signedConstant(outputConstant))
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionThresholdWithZeroFill") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .zeroFill)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/SinglePrecisionLimit") {
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
        
        let returnedResult = vDSP.limit(source,
                                        limit: bounds.upperBound,
                                        withOutputConstant: outputConstant)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP clipping, limit, and threshold tests; double-precision.
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    let bounds = Double(-0.5) ... Double(0.5)
    let outputConstant: Double = 99
    
    let source: [Double] = (0 ..< 256).map { i in
        return sin(Double(i) * 0.05) + sin(Double(i) * 0.025)
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionClipping") {
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
        
        let returnedResult = vDSP.clip(source,
                                       to: bounds)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionInvertedClipping") {
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
        
        let returnedResult = vDSP.invertedClip(source,
                                               to: bounds)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionThreshold") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .clampToThreshold)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionThresholdWithConstant") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .signedConstant(outputConstant))
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionThresholdWithZeroFill") {
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
        
        let returnedResult = vDSP.threshold(source,
                                            to: bounds.lowerBound,
                                            with: .zeroFill)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPClippingLimitThresholdTests.test("vDSP/DoublePrecisionLimit") {
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
        
        let returnedResult = vDSP.limit(source,
                                        limit: bounds.upperBound,
                                        withOutputConstant: outputConstant)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

runAllTests()
