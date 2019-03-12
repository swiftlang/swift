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
//  vDSP polar / rectangular conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("vDSP/SinglePrecisionRectToPolar") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionPolarToRect") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionRectToPolar") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionRectToPolar") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP decibel conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("vDSP/PowerToDecibelsSinglePrecision") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/AmplitudeToDecibelsSinglePrecision") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/PowerToDecibelsDoublePrecision") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/AmplitudeToDecibelsDoublePrecision") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP complex format conversion
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("vDSP/ComplexFormatConversionSinglePrecision") {
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
    
    AccelerateTests.test("vDSP/ComplexFormatConversionDoublePrecision") {
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
