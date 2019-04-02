// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var AccelerateTests_vDSPConversion = TestSuite("Accelerate_vDSPConversion")


//===----------------------------------------------------------------------===//
//
//  vDSP polar / rectangular conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests_vDSPConversion.test("vDSP/SinglePrecisionRectToPolar") {
        let source: [Float] = [2, 4,
                               -10, -20,
                               100, -300]
        
        let n = vDSP_Length(source.count)
        
        var result = [Float](repeating: 0,
                             count: source.count)
        
        vDSP.convert(rectangularCoordinates: source,
                     toPolarCoordinates: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: source.count)
        
        vDSP_polar(source, 2,
                   &legacyResult, 2,
                   n / 2)
        
        let returnedResult = vDSP.rectangularToPolar(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/SinglePrecisionPolarToRect") {
        let source: [Float] = [2, 4,
                               -10, -20,
                               100, -300]
        
        let n = vDSP_Length(source.count)
        
        var result = [Float](repeating: 0,
                             count: source.count)
        
        vDSP.convert(polarCoordinates: source,
                     toRectangularCoordinates: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: source.count)
        
        vDSP_rect(source, 2,
                  &legacyResult, 2,
                  n / 2)
        
        let returnedResult = vDSP.polarToRectangular(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoublePrecisionRectToPolar") {
        let source: [Double] = [2, 4,
                                -10, -20,
                                100, -300]
        
        let n = vDSP_Length(source.count)
        
        var result = [Double](repeating: 0,
                              count: source.count)
        
        vDSP.convert(rectangularCoordinates: source,
                     toPolarCoordinates: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: source.count)
        
        vDSP_polarD(source, 2,
                    &legacyResult, 2,
                    n / 2)
        
        let returnedResult = vDSP.rectangularToPolar(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoublePrecisionRectToPolar") {
        let source: [Double] = [2, 4,
                                -10, -20,
                                100, -300]
        
        let n = vDSP_Length(source.count)
        
        var result = [Double](repeating: 0,
                              count: source.count)
        
        vDSP.convert(polarCoordinates: source,
                     toRectangularCoordinates: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: source.count)
        
        vDSP_rectD(source, 2,
                   &legacyResult, 2,
                   n / 2)
        
        let returnedResult = vDSP.polarToRectangular(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP decibel conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests_vDSPConversion.test("vDSP/PowerToDecibelsSinglePrecision") {
        let source: [Float] = [1, 10, 100, 2, 4, 8, 16, 101,
                               27, 13, 11, 44, 0.5, 99, 0.125]
        let zeroReference = Float(7)
        let n = source.count
        
        var result = [Float](repeating: 0,
                             count: n)
        
        vDSP.convert(power: source,
                     toDecibels: &result,
                     zeroReference: zeroReference)
        
        var legacyResult = [Float](repeating: -1,
                                   count: n)
        
        vDSP_vdbcon(source, 1,
                    [zeroReference],
                    &legacyResult, 1,
                    vDSP_Length(n),
                    0)
        
        let returnedResult = vDSP.powerToDecibels(source,
                                                  zeroReference: zeroReference)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/AmplitudeToDecibelsSinglePrecision") {
        let source: [Float] = [1, 10, 100, 2, 4, 8, 16, 101,
                               27, 13, 11, 44, 0.5, 99, 0.125]
        let zeroReference = Float(7)
        let n = source.count
        
        var result = [Float](repeating: 0,
                             count: n)
        
        vDSP.convert(amplitude: source,
                     toDecibels: &result,
                     zeroReference: zeroReference)
        
        var legacyResult = [Float](repeating: -1,
                                   count: n)
        
        vDSP_vdbcon(source, 1,
                    [zeroReference],
                    &legacyResult, 1,
                    vDSP_Length(n),
                    1)
        
        let returnedResult = vDSP.amplitudeToDecibels(source,
                                                      zeroReference: zeroReference)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/PowerToDecibelsDoublePrecision") {
        let source: [Double] = [1, 10, 100, 2, 4, 8, 16, 101,
                                27, 13, 11, 44, 0.5, 99, 0.125]
        let zeroReference = Double(7)
        let n = source.count
        
        var result = [Double](repeating: 0,
                              count: n)
        
        vDSP.convert(power: source,
                     toDecibels: &result,
                     zeroReference: zeroReference)
        
        var legacyResult = [Double](repeating: -1,
                                    count: n)
        
        vDSP_vdbconD(source, 1,
                     [zeroReference],
                     &legacyResult, 1,
                     vDSP_Length(n),
                     0)
        
        let returnedResult = vDSP.powerToDecibels(source,
                                                  zeroReference: zeroReference)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/AmplitudeToDecibelsDoublePrecision") {
        let source: [Double] = [1, 10, 100, 2, 4, 8, 16, 101,
                                27, 13, 11, 44, 0.5, 99, 0.125]
        let zeroReference = Double(7)
        let n = source.count
        
        var result = [Double](repeating: 0,
                              count: n)
        
        vDSP.convert(amplitude: source,
                     toDecibels: &result,
                     zeroReference: zeroReference)
        
        var legacyResult = [Double](repeating: -1,
                                    count: n)
        
        vDSP_vdbconD(source, 1,
                     [zeroReference],
                     &legacyResult, 1,
                     vDSP_Length(n),
                     1)
        
        let returnedResult = vDSP.amplitudeToDecibels(source,
                                                      zeroReference: zeroReference)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP complex format conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests_vDSPConversion.test("vDSP/ComplexFormatConversionSinglePrecision") {
        var real: [Float] = [4, 5, 6]
        var imag: [Float] = [1, 2, 3]
        let src = DSPSplitComplex(realp: &real,
                                  imagp: &imag)
        
        var dest = [DSPComplex](repeating: DSPComplex(),
                                count: 3)
        
        vDSP.convert(splitComplexVector: src,
                     toInterleavedComplexVector: &dest)
        
        var realResult: [Float] = [0, 0, 0]
        var imagResult: [Float] = [0, 0, 0]
        var result = DSPSplitComplex(realp: &realResult,
                                     imagp: &imagResult)
        
        vDSP.convert(interleavedComplexVector: dest,
                     toSplitComplexVector: &result)
        
        expectTrue(real.elementsEqual(realResult))
        expectTrue(imag.elementsEqual(imagResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/ComplexFormatConversionDoublePrecision") {
        var real: [Double] = [4, 5, 6]
        var imag: [Double] = [1, 2, 3]
        let src = DSPDoubleSplitComplex(realp: &real,
                                        imagp: &imag)
        
        var dest = [DSPDoubleComplex](repeating: DSPDoubleComplex(),
                                      count: 3)
        
        vDSP.convert(splitComplexVector: src,
                     toInterleavedComplexVector: &dest)
        
        var realResult: [Double] = [0, 0, 0]
        var imagResult: [Double] = [0, 0, 0]
        var result = DSPDoubleSplitComplex(realp: &realResult,
                                           imagp: &imagResult)
        
        vDSP.convert(interleavedComplexVector: dest,
                     toSplitComplexVector: &result)
        
        expectTrue(real.elementsEqual(realResult))
        expectTrue(imag.elementsEqual(imagResult))
    }    
}

runAllTests()
