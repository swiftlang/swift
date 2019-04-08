// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPFourierTransformTests = TestSuite("Accelerate_vDSPFourierTransform")

//===----------------------------------------------------------------------===//
//
//  vDSP discrete Fourier transform tests; single-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = 2048
    let tau: Float = .pi * 2
    
    let frequencies: [Float] = [1, 5, 25, 30, 75, 100,
                                300, 500, 512, 1023]
    
    let inputReal: [Float] = (0 ..< n).map { index in
        frequencies.reduce(0) { accumulator, frequency in
            let normalizedIndex = Float(index) / Float(n)
            return accumulator + sin(normalizedIndex * frequency * tau)
        }
    }
    
    let inputImag: [Float] = (0 ..< n).map { index in
        frequencies.reduce(0) { accumulator, frequency in
            let normalizedIndex = Float(index) / Float(n)
            return accumulator + sin(normalizedIndex * 1/frequency * tau)
        }
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/SinglePrecisionForwardComplexComplex") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .forward,
                              transformType: .complexComplex,
                              ofType: Float.self)!
        
        var outputReal = [Float](repeating: 0, count: n)
        var outputImag = [Float](repeating: 0, count: n)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zop_CreateSetup(nil,
                                                   vDSP_Length(n),
                                                   .FORWARD)!
        
        var legacyOutputReal = [Float](repeating: -1, count: n)
        var legacyOutputImag = [Float](repeating: -1, count: n)
        
        vDSP_DFT_Execute(legacySetup,
                         inputReal,
                         inputImag,
                         &legacyOutputReal,
                         &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/SinglePrecisionInverseComplexComplex") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .inverse,
                              transformType: .complexComplex,
                              ofType: Float.self)!
        
        var outputReal = [Float](repeating: 0, count: n)
        var outputImag = [Float](repeating: 0, count: n)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zop_CreateSetup(nil,
                                                   vDSP_Length(n),
                                                   .INVERSE)!
        
        var legacyOutputReal = [Float](repeating: -1, count: n)
        var legacyOutputImag = [Float](repeating: -1, count: n)
        
        vDSP_DFT_Execute(legacySetup,
                         inputReal,
                         inputImag,
                         &legacyOutputReal,
                         &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/SinglePrecisionForwardComplexReal") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .forward,
                              transformType: .complexReal,
                              ofType: Float.self)!
        
        var outputReal = [Float](repeating: 0, count: n / 2)
        var outputImag = [Float](repeating: 0, count: n / 2)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zrop_CreateSetup(nil,
                                                    vDSP_Length(n),
                                                    .FORWARD)!
        
        var legacyOutputReal = [Float](repeating: -1, count: n / 2)
        var legacyOutputImag = [Float](repeating: -1, count: n / 2)
        
        vDSP_DFT_Execute(legacySetup,
                         inputReal,
                         inputImag,
                         &legacyOutputReal,
                         &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/SinglePrecisionInverseComplexReal") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .inverse,
                              transformType: .complexReal,
                              ofType: Float.self)!
        
        var outputReal = [Float](repeating: 0, count: n / 2)
        var outputImag = [Float](repeating: 0, count: n / 2)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zrop_CreateSetup(nil,
                                                    vDSP_Length(n),
                                                    .INVERSE)!
        
        var legacyOutputReal = [Float](repeating: -1, count: n / 2)
        var legacyOutputImag = [Float](repeating: -1, count: n / 2)
        
        vDSP_DFT_Execute(legacySetup,
                         inputReal,
                         inputImag,
                         &legacyOutputReal,
                         &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
}


//===----------------------------------------------------------------------===//
//
//  vDSP discrete Fourier transform tests; double-precision
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = 2048
    let tau: Double = .pi * 2
    
    let frequencies: [Double] = [1, 5, 25, 30, 75, 100,
                                 300, 500, 512, 1023]
    
    let inputReal: [Double] = (0 ..< n).map { index in
        frequencies.reduce(0) { accumulator, frequency in
            let normalizedIndex = Double(index) / Double(n)
            return accumulator + sin(normalizedIndex * frequency * tau)
        }
    }
    
    let inputImag: [Double] = (0 ..< n).map { index in
        frequencies.reduce(0) { accumulator, frequency in
            let normalizedIndex = Double(index) / Double(n)
            return accumulator + sin(normalizedIndex * 1/frequency * tau)
        }
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/DoublePrecisionForwardComplexComplex") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .forward,
                              transformType: .complexComplex,
                              ofType: Double.self)!
        
        var outputReal = [Double](repeating: 0, count: n)
        var outputImag = [Double](repeating: 0, count: n)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zop_CreateSetupD(nil,
                                                    vDSP_Length(n),
                                                    .FORWARD)!
        
        var legacyOutputReal = [Double](repeating: -1, count: n)
        var legacyOutputImag = [Double](repeating: -1, count: n)
        
        vDSP_DFT_ExecuteD(legacySetup,
                          inputReal,
                          inputImag,
                          &legacyOutputReal,
                          &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/DoublePrecisionInverseComplexComplex") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .inverse,
                              transformType: .complexComplex,
                              ofType: Double.self)!
        
        var outputReal = [Double](repeating: 0, count: n)
        var outputImag = [Double](repeating: 0, count: n)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zop_CreateSetupD(nil,
                                                    vDSP_Length(n),
                                                    .INVERSE)!
        
        var legacyOutputReal = [Double](repeating: -1, count: n)
        var legacyOutputImag = [Double](repeating: -1, count: n)
        
        vDSP_DFT_ExecuteD(legacySetup,
                          inputReal,
                          inputImag,
                          &legacyOutputReal,
                          &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/DoublePrecisionForwardComplexReal") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .forward,
                              transformType: .complexReal,
                              ofType: Double.self)!
        
        var outputReal = [Double](repeating: 0, count: n / 2)
        var outputImag = [Double](repeating: 0, count: n / 2)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zrop_CreateSetupD(nil,
                                                     vDSP_Length(n),
                                                     .FORWARD)!
        
        var legacyOutputReal = [Double](repeating: -1, count: n / 2)
        var legacyOutputImag = [Double](repeating: -1, count: n / 2)
        
        vDSP_DFT_ExecuteD(legacySetup,
                          inputReal,
                          inputImag,
                          &legacyOutputReal,
                          &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/DoublePrecisionInverseComplexReal") {
        let fwdDFT = vDSP.DFT(count: n,
                              direction: .inverse,
                              transformType: .complexReal,
                              ofType: Double.self)!
        
        var outputReal = [Double](repeating: 0, count: n / 2)
        var outputImag = [Double](repeating: 0, count: n / 2)
        
        fwdDFT.transform(inputReal: inputReal,
                         inputImaginary: inputImag,
                         outputReal: &outputReal,
                         outputImaginary: &outputImag)
        
        // legacy...
        
        let legacySetup = vDSP_DFT_zrop_CreateSetupD(nil,
                                                     vDSP_Length(n),
                                                     .INVERSE)!
        
        var legacyOutputReal = [Double](repeating: -1, count: n / 2)
        var legacyOutputImag = [Double](repeating: -1, count: n / 2)
        
        vDSP_DFT_ExecuteD(legacySetup,
                          inputReal,
                          inputImag,
                          &legacyOutputReal,
                          &legacyOutputImag)
        
        expectTrue(outputReal.elementsEqual(legacyOutputReal))
        expectTrue(outputImag.elementsEqual(legacyOutputImag))
        
        let returnedResult = fwdDFT.transform(inputReal: inputReal,
                                              inputImaginary: inputImag)
        
        expectTrue(outputReal.elementsEqual(returnedResult.real))
        expectTrue(outputImag.elementsEqual(returnedResult.imaginary))
    }
}

runAllTests()
