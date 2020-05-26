// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
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

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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

//===----------------------------------------------------------------------===//
//
//  vDSP Fast Fourier Transform Tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
  
    Accelerate_vDSPFourierTransformTests.test("vDSP/2DSinglePrecision") {
        let width = 256
        let height = 256
        let pixelCount = width * height
        let n = pixelCount / 2
        
        let pixels: [Float] = (0 ..< pixelCount).map { i in
            return abs(sin(Float(i) * 0.001 * 2))
        }
        
        var sourceImageReal = [Float](repeating: 0, count: n)
        var sourceImageImaginary = [Float](repeating: 0, count: n)
        
        sourceImageReal.withUnsafeMutableBufferPointer { sourceImageRealPtr in
            sourceImageImaginary.withUnsafeMutableBufferPointer { sourceImageImaginaryPtr in
                
                var sourceImage = DSPSplitComplex(realp: sourceImageRealPtr.baseAddress!,
                                                  imagp: sourceImageImaginaryPtr.baseAddress!)
                
                pixels.withUnsafeBytes{
                    vDSP_ctoz([DSPComplex]($0.bindMemory(to: DSPComplex.self)), 2,
                              &sourceImage, 1,
                              vDSP_Length(pixels.count / 2))
                }
                
                // Create FFT2D object
                let fft2D = vDSP.FFT2D(width: 256,
                                       height: 256,
                                       ofType: DSPSplitComplex.self)!
                
                // New style transform
                var transformedImageReal = [Float](repeating: 0,
                                                   count: n)
                var transformedImageImaginary = [Float](repeating: 0,
                                                        count: n)
                
                transformedImageReal.withUnsafeMutableBufferPointer { transformedImageRealPtr in
                    transformedImageImaginary.withUnsafeMutableBufferPointer { transformedImageImaginaryPtr in
                        
                        var transformedImage = DSPSplitComplex(
                            realp: transformedImageRealPtr.baseAddress!,
                            imagp: transformedImageImaginaryPtr.baseAddress!)
                        
                        fft2D.transform(input: sourceImage,
                                        output: &transformedImage,
                                        direction: .forward)
                    }
                }
  
                // Legacy 2D FFT
                
                let log2n = vDSP_Length(log2(Float(width * height)))
                let legacySetup = vDSP_create_fftsetup(
                    log2n,
                    FFTRadix(kFFTRadix2))!
                
                var legacyTransformedImageReal = [Float](repeating: -1,
                                                         count: n)
                var legacyTransformedImageImaginary = [Float](repeating: -1,
                                                              count: n)
                
                legacyTransformedImageReal.withUnsafeMutableBufferPointer { legacyTransformedImageRealPtr in
                    legacyTransformedImageImaginary.withUnsafeMutableBufferPointer { legacyTransformedImageImaginaryPtr in
                        
                        var legacyTransformedImage = DSPSplitComplex(
                            realp: legacyTransformedImageRealPtr.baseAddress!,
                            imagp: legacyTransformedImageImaginaryPtr.baseAddress!)
                        
                        vDSP_fft2d_zrop(legacySetup,
                                        &sourceImage, 1, 0,
                                        &legacyTransformedImage, 1, 0,
                                        vDSP_Length(log2(Float(width))),
                                        vDSP_Length(log2(Float(height))),
                                        FFTDirection(kFFTDirection_Forward))
                    }
                }
                
                expectTrue(transformedImageReal.elementsEqual(legacyTransformedImageReal))
                expectTrue(transformedImageImaginary.elementsEqual(legacyTransformedImageImaginary))
            }
        }
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/2DDoublePrecision") {
        let width = 256
        let height = 256
        let pixelCount = width * height
        let n = pixelCount / 2
        
        let pixels: [Double] = (0 ..< pixelCount).map { i in
            return abs(sin(Double(i) * 0.001 * 2))
        }
        
        var sourceImageReal = [Double](repeating: 0, count: n)
        var sourceImageImaginary = [Double](repeating: 0, count: n)
        
        sourceImageReal.withUnsafeMutableBufferPointer { sourceImageRealPtr in
            sourceImageImaginary.withUnsafeMutableBufferPointer { sourceImageImaginaryPtr in
                
                var sourceImage = DSPDoubleSplitComplex(realp: sourceImageRealPtr.baseAddress!,
                                                        imagp: sourceImageImaginaryPtr.baseAddress!)
                
                pixels.withUnsafeBytes{
                    vDSP_ctozD([DSPDoubleComplex]($0.bindMemory(to: DSPDoubleComplex.self)), 2,
                               &sourceImage, 1,
                               vDSP_Length(pixels.count / 2))
                }
                
                // Create FFT2D object
                let fft2D = vDSP.FFT2D(width: 256,
                                       height: 256,
                                       ofType: DSPDoubleSplitComplex.self)!
                
                // New style transform
                var transformedImageReal = [Double](repeating: 0,
                                                    count: n)
                var transformedImageImaginary = [Double](repeating: 0,
                                                         count: n)
                
                transformedImageReal.withUnsafeMutableBufferPointer { transformedImageRealPtr in
                    transformedImageImaginary.withUnsafeMutableBufferPointer { transformedImageImaginaryPtr in
                        
                        var transformedImage = DSPDoubleSplitComplex(
                            realp: transformedImageRealPtr.baseAddress!,
                            imagp: transformedImageImaginaryPtr.baseAddress!)
                        
                        fft2D.transform(input: sourceImage,
                                        output: &transformedImage,
                                        direction: .forward)
                    }
                }
   
                // Legacy 2D FFT
                
                let log2n = vDSP_Length(log2(Float(width * height)))
                let legacySetup = vDSP_create_fftsetupD(
                    log2n,
                    FFTRadix(kFFTRadix2))!
                
                var legacyTransformedImageReal = [Double](repeating: -1,
                                                          count: n)
                var legacyTransformedImageImaginary = [Double](repeating: -1,
                                                               count: n)
                
                legacyTransformedImageReal.withUnsafeMutableBufferPointer { legacyTransformedImageRealPtr in
                    legacyTransformedImageImaginary.withUnsafeMutableBufferPointer { legacyTransformedImageImaginaryPtr in
                        
                        var legacyTransformedImage = DSPDoubleSplitComplex(
                            realp: legacyTransformedImageRealPtr.baseAddress!,
                            imagp: legacyTransformedImageImaginaryPtr.baseAddress!)
                        
                        vDSP_fft2d_zropD(legacySetup,
                                         &sourceImage, 1, 0,
                                         &legacyTransformedImage, 1, 0,
                                         vDSP_Length(log2(Float(width))),
                                         vDSP_Length(log2(Float(height))),
                                         FFTDirection(kFFTDirection_Forward))
                    }
                }
                
                expectTrue(transformedImageReal.elementsEqual(legacyTransformedImageReal))
                expectTrue(transformedImageImaginary.elementsEqual(legacyTransformedImageImaginary))
            }
        }
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/1DSinglePrecision") {
        let n = vDSP_Length(2048)
          
          let frequencies: [Float] = [1, 5, 25, 30, 75, 100,
                                      300, 500, 512, 1023]
          
          let tau: Float = .pi * 2
          let signal: [Float] = (0 ... n).map { index in
              frequencies.reduce(0) { accumulator, frequency in
                  let normalizedIndex = Float(index) / Float(n)
                  return accumulator + sin(normalizedIndex * frequency * tau)
              }
          }
          
          let halfN = Int(n / 2)
          
          var forwardInputReal = [Float](repeating: 0, count: halfN)
          var forwardInputImag = [Float](repeating: 0, count: halfN)
          
          forwardInputReal.withUnsafeMutableBufferPointer { forwardInputRealPtr in
              forwardInputImag.withUnsafeMutableBufferPointer { forwardInputImagPtr in
                  
                  var forwardInput = DSPSplitComplex(realp: forwardInputRealPtr.baseAddress!,
                                                     imagp: forwardInputImagPtr.baseAddress!)
                  
                  signal.withUnsafeBytes{
                      vDSP_ctoz([DSPComplex]($0.bindMemory(to: DSPComplex.self)), 2,
                                &forwardInput, 1,
                                vDSP_Length(signal.count / 2))
                  }
                  
                  let log2n = vDSP_Length(log2(Float(n)))
                  
                  // New API
                  
                  guard let fft = vDSP.FFT(log2n: log2n,
                                           radix: .radix2,
                                           ofType: DSPSplitComplex.self) else {
                                              fatalError("Can't create FFT.")
                  }
                  
                  var outputReal = [Float](repeating: 0, count: halfN)
                  var outputImag = [Float](repeating: 0, count: halfN)
                  
                  outputReal.withUnsafeMutableBufferPointer { outputRealPtr in
                      outputImag.withUnsafeMutableBufferPointer { outputImagPtr in
                          var forwardOutput = DSPSplitComplex(realp: outputRealPtr.baseAddress!,
                                                              imagp: outputImagPtr.baseAddress!)
                          
                          fft.transform(input: forwardInput,
                                        output: &forwardOutput,
                                        direction: .forward)
                      }
                  }
                  
                  // Legacy Style
                  
                  let legacySetup = vDSP_create_fftsetup(log2n,
                                                         FFTRadix(kFFTRadix2))!
                  
                  var legacyOutputReal = [Float](repeating: -1, count: halfN)
                  var legacyOutputImag = [Float](repeating: -1, count: halfN)
                  
                  legacyOutputReal.withUnsafeMutableBufferPointer { legacyOutputRealPtr in
                      legacyOutputImag.withUnsafeMutableBufferPointer { legacyOutputImagPtr in
                          var legacyForwardOutput = DSPSplitComplex(realp: legacyOutputRealPtr.baseAddress!,
                                                                    imagp: legacyOutputImagPtr.baseAddress!)
                          
                          vDSP_fft_zrop(legacySetup,
                                        &forwardInput, 1,
                                        &legacyForwardOutput, 1,
                                        log2n,
                                        FFTDirection(kFFTDirection_Forward))
                      }
                  }
                  
                  expectTrue(outputReal.elementsEqual(legacyOutputReal))
                  expectTrue(outputImag.elementsEqual(legacyOutputImag))
              }
          }
    }
    
    Accelerate_vDSPFourierTransformTests.test("vDSP/1DDoublePrecision") {
        let n = vDSP_Length(2048)
        
        let frequencies: [Double] = [1, 5, 25, 30, 75, 100,
                                     300, 500, 512, 1023]
        
        let tau: Double = .pi * 2
        let signal: [Double] = (0 ... n).map { index in
            frequencies.reduce(0) { accumulator, frequency in
                let normalizedIndex = Double(index) / Double(n)
                return accumulator + sin(normalizedIndex * frequency * tau)
            }
        }
        
        let halfN = Int(n / 2)
        
        var forwardInputReal = [Double](repeating: 0, count: halfN)
        var forwardInputImag = [Double](repeating: 0, count: halfN)
        
        forwardInputReal.withUnsafeMutableBufferPointer { forwardInputRealPtr in
            forwardInputImag.withUnsafeMutableBufferPointer { forwardInputImagPtr in
                
                var forwardInput = DSPDoubleSplitComplex(realp: forwardInputRealPtr.baseAddress!,
                                                         imagp: forwardInputImagPtr.baseAddress!)
                
                signal.withUnsafeBytes{
                    vDSP_ctozD([DSPDoubleComplex]($0.bindMemory(to: DSPDoubleComplex.self)), 2,
                               &forwardInput, 1,
                               vDSP_Length(signal.count / 2))
                }
                
                let log2n = vDSP_Length(log2(Float(n)))
                
                // New API
                
                guard let fft = vDSP.FFT(log2n: log2n,
                                         radix: .radix2,
                                         ofType: DSPDoubleSplitComplex.self) else {
                                            fatalError("Can't create FFT.")
                }
                
                var outputReal = [Double](repeating: 0, count: halfN)
                var outputImag = [Double](repeating: 0, count: halfN)
                
                outputReal.withUnsafeMutableBufferPointer { outputRealPtr in
                    outputImag.withUnsafeMutableBufferPointer { outputImagPtr in
                        var forwardOutput = DSPDoubleSplitComplex(realp: outputRealPtr.baseAddress!,
                                                                  imagp: outputImagPtr.baseAddress!)
                        
                        fft.transform(input: forwardInput,
                                      output: &forwardOutput,
                                      direction: .forward)
                    }
                }
                
                // Legacy Style
                
                let legacySetup = vDSP_create_fftsetupD(log2n,
                                                        FFTRadix(kFFTRadix2))!
                
                var legacyOutputReal = [Double](repeating: -1, count: halfN)
                var legacyOutputImag = [Double](repeating: -1, count: halfN)
                
                legacyOutputReal.withUnsafeMutableBufferPointer { legacyOutputRealPtr in
                    legacyOutputImag.withUnsafeMutableBufferPointer { legacyOutputImagPtr in
                        var legacyForwardOutput = DSPDoubleSplitComplex(realp: legacyOutputRealPtr.baseAddress!,
                                                                        imagp: legacyOutputImagPtr.baseAddress!)
                        
                        vDSP_fft_zropD(legacySetup,
                                       &forwardInput, 1,
                                       &legacyForwardOutput, 1,
                                       log2n,
                                       FFTDirection(kFFTDirection_Forward))
                    }
                }
                
                expectTrue(outputReal.elementsEqual(legacyOutputReal))
                expectTrue(outputImag.elementsEqual(legacyOutputImag))
            }
        }
    }
    
}

runAllTests()
