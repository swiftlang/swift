// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPComplexOperationsTests = TestSuite("Accelerate_vDSPComplexOperations")

//===----------------------------------------------------------------------===//
//
//  Complex operations tests; single-precision
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexAbsolute") {
        vDSP.absolute(splitComplexSource,
                      result: &realResult)
        
        vDSP_zvabs(&splitComplexSource, 1,
                   &realLegacyResult, 1,
                   vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/SquareMagnitudes") {
        vDSP.squareMagnitudes(splitComplexSource,
                              result: &realResult)
        
        vDSP_zvmags(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Conjugate") {
        vDSP.conjugate(splitComplexSource,
                       count: n,
                       result: &splitComplexResult)
        
        vDSP_zvconj(&splitComplexSource, 1,
                    &splitComplexLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexAdd") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexDivide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexDivide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexSubtract") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexMultiply") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Multiply") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Divide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Phase") {
        vDSP.phase(splitComplexSource,
                   result: &realResult)
        
        vDSP_zvphas(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Copy") {
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

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexAbsolute") {
        vDSP.absolute(splitComplexSource,
                      result: &realResult)
        
        vDSP_zvabsD(&splitComplexSource, 1,
                    &realLegacyResult, 1,
                    vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/SquareMagnitudes") {
        vDSP.squareMagnitudes(splitComplexSource,
                              result: &realResult)
        
        vDSP_zvmagsD(&splitComplexSource, 1,
                     &realLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Conjugate") {
        vDSP.conjugate(splitComplexSource,
                       count: n,
                       result: &splitComplexResult)
        
        vDSP_zvconjD(&splitComplexSource, 1,
                     &splitComplexLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
        expectTrue(imaginaryResult.elementsEqual(imaginaryLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexAdd") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexDivide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexDivide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexSubtract") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/ComplexComplexMultiply") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Multiply") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Divide") {
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
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Phase") {
        vDSP.phase(splitComplexSource,
                   result: &realResult)
        
        vDSP_zvphasD(&splitComplexSource, 1,
                     &realLegacyResult, 1,
                     vDSP_Length(n))
        
        expectTrue(realResult.elementsEqual(realLegacyResult))
    }
    
    Accelerate_vDSPComplexOperationsTests.test("vDSP/Copy") {
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
