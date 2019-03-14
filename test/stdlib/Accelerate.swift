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
//  vDSP Extrema, absolute & negative, reversing, sorting
//  and single vector arithmetic; single-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 50
    let n = vDSP_Length(50)
    
    let a = (0 ..< 50).map {
        return sin(Float($0 * 3))
    }
    let b = (0 ..< 50).map {
        return sin(Float($0 * 7))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMin") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionMax") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionAbsolute") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.absolute(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vabs(a, 1,
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionNegativeAbsolute") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.negativeAbsolute(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vnabs(a, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionNegative") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.negative(a, result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vneg(a, 1,
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionReverse") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.reverse(&mutableA)
        
        vDSP_vrvrs(&mutableLegacyA, 1,
                   n)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionSort") {
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
    
    AccelerateTests.test("vDSP/SinglePrecisionSquare") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.square(a,
                    result: &result)
        
        vDSP_vsq(a, 1,
                 &legacyResult, 1,
                 n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionSignedSquare") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.signedSquare(a,
                          result: &result)
        
        vDSP_vssq(a, 1,
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionTruncateToFraction") {
        var result = [Float](repeating: 0, count: count)
        var legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.trunc(a,
                   result: &result)
        
        vDSP_vfrac(a, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionZeroCrossing") {
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

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 50
    let n = vDSP_Length(50)
    
    let a = (0 ..< 50).map {
        return sin(Double($0 * 3))
    }
    let b = (0 ..< 50).map {
        return sin(Double($0 * 7))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMin") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionMax") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionAbsolute") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.absolute(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vabsD(a, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionNegativeAbsolute") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.negativeAbsolute(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vnabsD(a, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionNegative") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.negative(a, result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vnegD(a, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionReverse") {
        var mutableA = a
        var mutableLegacyA = a
        
        vDSP.reverse(&mutableA)
        
        vDSP_vrvrsD(&mutableLegacyA, 1,
                    n)
        
        expectTrue(mutableA.elementsEqual(mutableLegacyA))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionSort") {
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
    
    AccelerateTests.test("vDSP/DoublePrecisionSquare") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.square(a,
                    result: &result)
        
        vDSP_vsqD(a, 1,
                  &legacyResult, 1,
                  n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionSignedSquare") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.signedSquare(a,
                          result: &result)
        
        vDSP_vssqD(a, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionTruncateToFraction") {
        var result = [Double](repeating: 0, count: count)
        var legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.trunc(a,
                   result: &result)
        
        vDSP_vfracD(a, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionZeroCrossing") {
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
