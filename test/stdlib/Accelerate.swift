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
//  Complex operations tests; single-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = 1024
    
    var imaginarySource: [Float] = (0 ..< n).map{ i in
        return 1 + sin(Float(i) * 0.05)
    }
    
    var realSource: [Float] = (0 ..< n).map{ i in
        return 1 + cos(Float(i) * 0.05)
    }
    
    var splitComplexSource = DSPSplitComplex(realp: &realSource,
                                             imagp: &imaginarySource)
    
    let vectorSource: [Float] = (0 ..< n).map{ i in
        return 1 + sin(Float(i) * 0.025)
    }
    
    var realResult = [Float](repeating: 0, count: n)
    var imaginaryResult = [Float](repeating: 0, count: n)
    var splitComplexResult = DSPSplitComplex(realp: &realResult,
                                             imagp: &imaginaryResult)
    
    var realLegacyResult = [Float](repeating: -1, count: n)
    var imaginaryLegacyResult = [Float](repeating: -1, count: n)
    var splitComplexLegacyResult = DSPSplitComplex(realp: &realLegacyResult,
                                                   imagp: &imaginaryLegacyResult)
    
    AccelerateTests.test("vDSP/ComplexAbsolute") {
        vDSP.absolute(splitComplexSource,
                      result: &realResult)
        
        vDSP_zvabs(&splitComplexSource, 1,
                   &realLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/SquareMagnitudes") {
        vDSP.squareMagnitudes(splitComplexSource,
                              result: &realResult)
        
        vDSP_zvmags(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Conjugate") {
        vDSP.conjugate(splitComplexSource,
                       count: n,
                       result: &splitComplexResult)
        
        vDSP_zvconj(&splitComplexSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexAdd") {
        var real = (0 ..< n).map{ i in
            return sin(Float(i) * 0.025)
        }
        var imag = (0 ..< n).map{ i in
            return cos(Float(i) * 0.025)
        }
        var splitComplexSource2 = DSPSplitComplex(realp: &real,
                                                  imagp: &imag)
        
        vDSP.add(splitComplexSource,
                 to: splitComplexSource2,
                 count: n,
                 result: &splitComplexResult)
        
        vDSP_zvadd(&splitComplexSource, 1,
                   &splitComplexSource2, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexDivide") {
        var real = (0 ..< n).map{ i in
            return sin(Float(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Float(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPSplitComplex(realp: &real,
                                                  imagp: &imag)
        
        vDSP.divide(splitComplexSource,
                    by: splitComplexSource2,
                    count: n,
                    result: &splitComplexResult)
        
        vDSP_zvdiv(&splitComplexSource2, 1,
                   &splitComplexSource, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexDivide") {
        var real = (0 ..< n).map{ i in
            return sin(Float(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Float(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPSplitComplex(realp: &real,
                                                  imagp: &imag)
        
        vDSP.divide(splitComplexSource,
                    by: splitComplexSource2,
                    count: n,
                    result: &splitComplexResult)
        
        vDSP_zvdiv(&splitComplexSource2, 1,
                   &splitComplexSource, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexSubtract") {
        var real = (0 ..< n).map{ i in
            return sin(Float(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Float(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPSplitComplex(realp: &real,
                                                  imagp: &imag)
        
        vDSP.subtract(splitComplexSource,
                      from: splitComplexSource2,
                      count: n,
                      result: &splitComplexResult)
        
        vDSP_zvsub(&splitComplexSource, 1,
                   &splitComplexSource2, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexMultiply") {
        var real = (0 ..< n).map{ i in
            return sin(Float(i) * 0.025)
        }
        var imag = (0 ..< n).map{ i in
            return cos(Float(i) * 0.025)
        }
        var splitComplexSource2 = DSPSplitComplex(realp: &real,
                                                  imagp: &imag)
        
        vDSP.multiply(splitComplexSource,
                      by: splitComplexSource2,
                      count: n,
                      useConjugate: false,
                      result: &splitComplexResult)
        
        vDSP_zvmul(&splitComplexSource, 1,
                   &splitComplexSource2, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n),
                   1)
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
        
        vDSP.multiply(splitComplexSource,
                      by: splitComplexSource2,
                      count: n,
                      useConjugate: true,
                      result: &splitComplexResult)
        
        vDSP_zvmul(&splitComplexSource, 1,
                   &splitComplexSource2, 1,
                   &splitComplexLegacyResult, 1,
                   vDSP_Length(n),
                   -1)
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Multiply") {
        vDSP.multiply(splitComplexSource,
                      by: vectorSource,
                      result: &splitComplexResult)
        
        vDSP_zrvmul(&splitComplexSource, 1,
                    vectorSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Divide") {
        vDSP.divide(splitComplexSource,
                    by: vectorSource,
                    result: &splitComplexResult)
        
        vDSP_zrvdiv(&splitComplexSource, 1,
                    vectorSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Phase") {
        vDSP.phase(splitComplexSource,
                   result: &realResult)
        
        vDSP_zvphas(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Copy") {
        expectFalse(imaginaryResult.elementsEqual(imaginarySource))
        expectFalse(realResult.elementsEqual(realSource))
        
        vDSP.copy(splitComplexSource,
                  to: &splitComplexResult,
                  count: n)
        
        expectTrue(imaginaryResult.elementsEqual(imaginarySource))
        expectTrue(realResult.elementsEqual(realSource))
    }
}


//===----------------------------------------------------------------------===//
//
//  Complex operations tests; double-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = 1024
    
    var imaginarySource: [Double] = (0 ..< n).map{ i in
        return 1 + sin(Double(i) * 0.05)
    }
    
    var realSource: [Double] = (0 ..< n).map{ i in
        return 1 + cos(Double(i) * 0.05)
    }
    
    var splitComplexSource = DSPDoubleSplitComplex(realp: &realSource,
                                                   imagp: &imaginarySource)
    
    let vectorSource: [Double] = (0 ..< n).map{ i in
        return 1 + sin(Double(i) * 0.025)
    }
    
    var realResult = [Double](repeating: 0, count: n)
    var imaginaryResult = [Double](repeating: 0, count: n)
    var splitComplexResult = DSPDoubleSplitComplex(realp: &realResult,
                                                   imagp: &imaginaryResult)
    
    var realLegacyResult = [Double](repeating: -1, count: n)
    var imaginaryLegacyResult = [Double](repeating: -1, count: n)
    var splitComplexLegacyResult = DSPDoubleSplitComplex(realp: &realLegacyResult,
                                                         imagp: &imaginaryLegacyResult)
    
    AccelerateTests.test("vDSP/ComplexAbsolute") {
        vDSP.absolute(splitComplexSource,
                      result: &realResult)
        
        vDSP_zvabsD(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/SquareMagnitudes") {
        vDSP.squareMagnitudes(splitComplexSource,
                              result: &realResult)
        
        vDSP_zvmagsD(&splitComplexSource, 1,
                     &realLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Conjugate") {
        vDSP.conjugate(splitComplexSource,
                       count: n,
                       result: &splitComplexResult)
        
        vDSP_zvconjD(&splitComplexSource, 1,
                     &splitComplexLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexAdd") {
        var real = (0 ..< n).map{ i in
            return sin(Double(i) * 0.025)
        }
        var imag = (0 ..< n).map{ i in
            return cos(Double(i) * 0.025)
        }
        var splitComplexSource2 = DSPDoubleSplitComplex(realp: &real,
                                                        imagp: &imag)
        
        vDSP.add(splitComplexSource,
                 to: splitComplexSource2,
                 count: n,
                 result: &splitComplexResult)
        
        vDSP_zvaddD(&splitComplexSource, 1,
                    &splitComplexSource2, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexDivide") {
        var real = (0 ..< n).map{ i in
            return sin(Double(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Double(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPDoubleSplitComplex(realp: &real,
                                                        imagp: &imag)
        
        vDSP.divide(splitComplexSource,
                    by: splitComplexSource2,
                    count: n,
                    result: &splitComplexResult)
        
        vDSP_zvdivD(&splitComplexSource2, 1,
                    &splitComplexSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexDivide") {
        var real = (0 ..< n).map{ i in
            return sin(Double(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Double(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPDoubleSplitComplex(realp: &real,
                                                        imagp: &imag)
        
        vDSP.divide(splitComplexSource,
                    by: splitComplexSource2,
                    count: n,
                    result: &splitComplexResult)
        
        vDSP_zvdivD(&splitComplexSource2, 1,
                    &splitComplexSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexSubtract") {
        var real = (0 ..< n).map{ i in
            return sin(Double(i) * 0.025) + 2
        }
        var imag = (0 ..< n).map{ i in
            return cos(Double(i) * 0.025) + 2
        }
        var splitComplexSource2 = DSPDoubleSplitComplex(realp: &real,
                                                        imagp: &imag)
        
        vDSP.subtract(splitComplexSource,
                      from: splitComplexSource2,
                      count: n,
                      result: &splitComplexResult)
        
        vDSP_zvsubD(&splitComplexSource, 1,
                    &splitComplexSource2, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/ComplexComplexMultiply") {
        var real = (0 ..< n).map{ i in
            return sin(Double(i) * 0.025)
        }
        var imag = (0 ..< n).map{ i in
            return cos(Double(i) * 0.025)
        }
        var splitComplexSource2 = DSPDoubleSplitComplex(realp: &real,
                                                        imagp: &imag)
        
        vDSP.multiply(splitComplexSource,
                      by: splitComplexSource2,
                      count: n,
                      useConjugate: false,
                      result: &splitComplexResult)
        
        vDSP_zvmulD(&splitComplexSource, 1,
                    &splitComplexSource2, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n),
                    1)
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
        
        vDSP.multiply(splitComplexSource,
                      by: splitComplexSource2,
                      count: n,
                      useConjugate: true,
                      result: &splitComplexResult)
        
        vDSP_zvmulD(&splitComplexSource, 1,
                    &splitComplexSource2, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n),
                    -1)
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Multiply") {
        vDSP.multiply(splitComplexSource,
                      by: vectorSource,
                      result: &splitComplexResult)
        
        vDSP_zrvmulD(&splitComplexSource, 1,
                     vectorSource, 1,
                     &splitComplexLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Divide") {
        vDSP.divide(splitComplexSource,
                    by: vectorSource,
                    result: &splitComplexResult)
        
        vDSP_zrvdivD(&splitComplexSource, 1,
                     vectorSource, 1,
                     &splitComplexLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Phase") {
        vDSP.phase(splitComplexSource,
                   result: &realResult)
        
        vDSP_zvphasD(&splitComplexSource, 1,
                     &realLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    AccelerateTests.test("vDSP/Copy") {
        expectFalse(imaginaryResult.elementsEqual(imaginarySource))
        expectFalse(realResult.elementsEqual(realSource))
        
        vDSP.copy(splitComplexSource,
                  to: &splitComplexResult,
                  count: n)
        
        expectTrue(imaginaryResult.elementsEqual(imaginarySource))
        expectTrue(realResult.elementsEqual(realSource))
    }
}

runAllTests()
