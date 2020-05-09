// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPReductionTests = TestSuite("Accelerate_vDSPReduction")

//===----------------------------------------------------------------------===//
//
//  vDSP vector reduction; single-precision
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let n = vDSP_Length(256)
    
    let a = (0 ..< 256).map {
        return sin(Float($0) * 0.03) * cos(Float($0) * 0.07)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMaximum") {
        let result: Float = vDSP.maximum(a)
        
        var legacyResult = Float(0)
        
        vDSP_maxv(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMaximumIndex") {
        let result: (vDSP_Length, Float) = vDSP.indexOfMaximum(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxvi(a, 1,
                   &legacyValueResult,
                   &legacyIndexResult,
                   n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMaximumMagnitude") {
        let result: Float = vDSP.maximumMagnitude(a)
        
        var legacyResult = Float(0)
        
        vDSP_maxmgv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMaximumMagnitudeIndex") {
        let result: (vDSP_Length, Float) = vDSP.indexOfMaximumMagnitude(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxmgvi(a, 1,
                     &legacyValueResult,
                     &legacyIndexResult,
                     n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMaximumMagnitudeIndex") {
        let result: Float = vDSP.minimum(a)
        
        var legacyResult = Float(0)
        
        vDSP_minv(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionMinimumIndex") {
        let result: (vDSP_Length, Float) = vDSP.indexOfMinimum(a)
        
        var legacyValueResult = Float(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_minvi(a, 1,
                   &legacyValueResult,
                   &legacyIndexResult,
                   n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionSum") {
        let result: Float = vDSP.sum(a)
        
        var legacyResult = Float(0)
        
        vDSP_sve(a, 1,
                 &legacyResult,
                 n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionSumOfSquares") {
        let result: Float = vDSP.sumOfSquares(a)
        
        var legacyResult = Float(0)
        
        vDSP_svesq(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionSumOfElementsAndSquares") {
        let result: (Float, Float) = vDSP.sumAndSumOfSquares(a)
        
        var legacySumResult = Float(0)
        var legacySumOfSquaresResult = Float(0)
        
        vDSP_sve_svesq(a, 1,
                       &legacySumResult,
                       &legacySumOfSquaresResult,
                       n)
        
        expectEqual(result.0, legacySumResult)
        expectEqual(result.1, legacySumOfSquaresResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/SinglePrecisionSumOfMagnitudes") {
        let result: Float = vDSP.sumOfMagnitudes(a)
        
        var legacyResult = Float(0)
        
        vDSP_svemg(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.meanSquare(a)
        
        var legacyResult = Float.nan
        
        vDSP_measqv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.meanMagnitude(a)
        
        var legacyResult = Float.nan
        
        vDSP_meamgv(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareSinglePrecision") {
        let result = vDSP.mean(a)
        
        var legacyResult = Float.nan
        
        vDSP_meanv(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/RMSSinglePrecision") {
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

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let n = vDSP_Length(256)
    
    let a = (0 ..< 256).map {
        return sin(Double($0) * 0.03) * cos(Double($0) * 0.07)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMaximum") {
        let result: Double = vDSP.maximum(a)
        
        var legacyResult = Double(0)
        
        vDSP_maxvD(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMaximumIndex") {
        let result: (vDSP_Length, Double) = vDSP.indexOfMaximum(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxviD(a, 1,
                    &legacyValueResult,
                    &legacyIndexResult,
                    n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMaximumMagnitude") {
        let result: Double = vDSP.maximumMagnitude(a)
        
        var legacyResult = Double(0)
        
        vDSP_maxmgvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMaximumMagnitudeIndex") {
        let result: (vDSP_Length, Double) = vDSP.indexOfMaximumMagnitude(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_maxmgviD(a, 1,
                      &legacyValueResult,
                      &legacyIndexResult,
                      n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMaximumMagnitudeIndex") {
        let result: Double = vDSP.minimum(a)
        
        var legacyResult = Double(0)
        
        vDSP_minvD(a, 1,
                   &legacyResult,
                   n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionMinimumIndex") {
        let result: (vDSP_Length, Double) = vDSP.indexOfMinimum(a)
        
        var legacyValueResult = Double(0)
        var legacyIndexResult = vDSP_Length(0)
        
        vDSP_minviD(a, 1,
                    &legacyValueResult,
                    &legacyIndexResult,
                    n)
        
        expectEqual(result.1, legacyValueResult)
        expectEqual(result.0, legacyIndexResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionSum") {
        let result: Double = vDSP.sum(a)
        
        var legacyResult = Double(0)
        
        vDSP_sveD(a, 1,
                  &legacyResult,
                  n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionSumOfSquares") {
        let result: Double = vDSP.sumOfSquares(a)
        
        var legacyResult = Double(0)
        
        vDSP_svesqD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionSumOfElementsAndSquares") {
        let result: (Double, Double) = vDSP.sumAndSumOfSquares(a)
        
        var legacySumResult = Double(0)
        var legacySumOfSquaresResult = Double(0)
        
        vDSP_sve_svesqD(a, 1,
                        &legacySumResult,
                        &legacySumOfSquaresResult,
                        n)
        
        expectEqual(result.0, legacySumResult)
        expectEqual(result.1, legacySumOfSquaresResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/DoublePrecisionSumOfMagnitudes") {
        let result: Double = vDSP.sumOfMagnitudes(a)
        
        var legacyResult = Double(0)
        
        vDSP_svemgD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.meanSquare(a)
        
        var legacyResult = Double.nan
        
        vDSP_measqvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.meanMagnitude(a)
        
        var legacyResult = Double.nan
        
        vDSP_meamgvD(a, 1,
                     &legacyResult,
                     n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/MeanSquareDoublePrecision") {
        let result = vDSP.mean(a)
        
        var legacyResult = Double.nan
        
        vDSP_meanvD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
    
    Accelerate_vDSPReductionTests.test("vDSP/RMSDoublePrecision") {
        let result = vDSP.rootMeanSquare(a)
        
        var legacyResult = Double.nan
        
        vDSP_rmsqvD(a, 1,
                    &legacyResult,
                    n)
        
        expectEqual(result, legacyResult)
    }
}

runAllTests()
