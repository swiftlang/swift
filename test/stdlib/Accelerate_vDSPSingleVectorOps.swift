// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPSingleVectorOpsTests = TestSuite("Accelerate_vDSPSingleVectorOps")

//===----------------------------------------------------------------------===//
//
//  vDSP Extrema, absolute & negative, reversing, sorting
//  and single vector arithmetic; single-precision
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 50
    let n = vDSP_Length(50)
    
    let a = (0 ..< 50).map {
        return sin(Float($0 * 3))
    }
    let b = (0 ..< 50).map {
        return sin(Float($0 * 7))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionMin") {
        var result = [Float](repeating: .nan,
                             count: count)
        
        vDSP.minimum(a,
                     b,
                     result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vmin(a, 1,
                  b, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.minimum(a,
                                          b)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionMax") {
        var result = [Float](repeating: .nan,
                             count: count)
        
        vDSP.maximum(a,
                     b,
                     result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vmax(a, 1,
                  b, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.maximum(a,
                                          b)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionAbsolute") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.absolute(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vabs(a, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.absolute(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionNegativeAbsolute") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.negativeAbsolute(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vnabs(a, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.negativeAbsolute(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionNegative") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.negative(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vneg(a, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.negative(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionReverse") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.reverse(&mutableA)
        
        vDSP_vrvrs(&mutableLegacyA, 1,
                   n)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionSort") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.sort(&mutableA,
                  sortOrder: .ascending)
        
        vDSP_vsort(&mutableLegacyA,
                   n,
                   1)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
        
        vDSP.sort(&mutableA,
                  sortOrder: .descending)
        
        vDSP_vsort(&mutableLegacyA,
                   n,
                   -1)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionSquare") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.square(a,
                    result: &result)
        
        vDSP_vsq(a, 1,
                 &legacyResult, 1,
                 n)
        
        let returnedResult = vDSP.square(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionSignedSquare") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.signedSquare(a,
                          result: &result)
        
        vDSP_vssq(a, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.signedSquare(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionTruncateToFraction") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.trunc(a,
                   result: &result)
        
        vDSP_vfrac(a, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.trunc(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/SinglePrecisionZeroCrossing") {
        let crossingCount = vDSP.countZeroCrossings(a)
        
        var legacyCrossingCount = vDSP_Length(0)
        var legacyLastCrossingIndex = vDSP_Length(0)
        
        vDSP_nzcros(a, 1,
                    n,
                    &legacyLastCrossingIndex,
                    &legacyCrossingCount,
                    n)
        
        expectEqual(crossingCount, legacyCrossingCount)
    }
    
}

//===----------------------------------------------------------------------===//
//
//  vDSP Extrema, absolute & negative, reversing, sorting
//  and single vector arithmetic; double-precision
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 50
    let n = vDSP_Length(50)
    
    let a = (0 ..< 50).map {
        return sin(Double($0 * 3))
    }
    let b = (0 ..< 50).map {
        return sin(Double($0 * 7))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionMin") {
        var result = [Double](repeating: .nan,
                              count: count)
        
        vDSP.minimum(a,
                     b,
                     result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vminD(a, 1,
                   b, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.minimum(a,
                                          b)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionMax") {
        var result = [Double](repeating: .nan,
                              count: count)
        
        vDSP.maximum(a,
                     b,
                     result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vmaxD(a, 1,
                   b, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.maximum(a,
                                          b)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionAbsolute") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.absolute(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vabsD(a, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.absolute(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionNegativeAbsolute") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.negativeAbsolute(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vnabsD(a, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.negativeAbsolute(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionNegative") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.negative(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vnegD(a, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.negative(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionReverse") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.reverse(&mutableA)
        
        vDSP_vrvrsD(&mutableLegacyA, 1,
                    n)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionSort") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.sort(&mutableA,
                  sortOrder: .ascending)
        
        vDSP_vsortD(&mutableLegacyA,
                    n,
                    1)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
        
        vDSP.sort(&mutableA,
                  sortOrder: .descending)
        
        vDSP_vsortD(&mutableLegacyA,
                    n,
                    -1)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionSquare") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.square(a,
                    result: &result)
        
        vDSP_vsqD(a, 1,
                  &legacyResult, 1,
                  n)
        
        let returnedResult = vDSP.square(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionSignedSquare") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.signedSquare(a,
                          result: &result)
        
        vDSP_vssqD(a, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.signedSquare(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionTruncateToFraction") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.trunc(a,
                   result: &result)
        
        vDSP_vfracD(a, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.trunc(a)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPSingleVectorOpsTests.test("vDSP/DoublePrecisionZeroCrossing") {
        let crossingCount = vDSP.countZeroCrossings(a)
        
        var legacyCrossingCount = vDSP_Length(0)
        var legacyLastCrossingIndex = vDSP_Length(0)
        
        vDSP_nzcrosD(a, 1,
                     n,
                     &legacyLastCrossingIndex,
                     &legacyCrossingCount,
                     n)
        
        expectEqual(crossingCount, legacyCrossingCount)
    }
}

runAllTests()
