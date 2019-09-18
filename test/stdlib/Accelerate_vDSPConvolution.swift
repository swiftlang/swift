
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPConvolutionTests = TestSuite("Accelerate_vDSPConvolution")

//===----------------------------------------------------------------------===//
//
//  vDSP Single-precision 1D convolution and correlation
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    
    let kernel:  [Float] = ( 0 ..< 10 ).map {
        return Float($0) / 45
    }
    
    let signal: [Float] = (0 ..< 256 + 10).map {
        return sin(Float($0) * 0.05).sign == .minus ? -1 : 1
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/SinglePrecisionConvolve") {
        var result = [Float](repeating: 0,
                             count: count)
        vDSP.convolve(signal,
                      withKernel: kernel,
                      result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_conv(signal, 1,
                  UnsafePointer(kernel).advanced(by: kernel.count - 1), -1,
                  &legacyResult, 1,
                  n,
                  vDSP_Length(kernel.count))
        
        let returnedResult = vDSP.convolve(signal,
                                           withKernel: kernel)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/SinglePrecisionCorrelate") {
        var result = [Float](repeating: 0,
                             count: count)
        vDSP.correlate(signal,
                       withKernel: kernel,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_conv(signal, 1,
                  kernel, 1,
                  &legacyResult, 1,
                  n,
                  vDSP_Length(kernel.count))
        
        let returnedResult = vDSP.correlate(signal,
                                           withKernel: kernel)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Double-precision 1D convolution and correlation
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    
    let kernel:  [Double] = ( 0 ..< 10 ).map {
        return Double($0) / 45
    }
    
    let signal: [Double] = (0 ..< 256 + 10).map {
        return sin(Double($0) * 0.05).sign == .minus ? -1 : 1
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/DoublePrecisionConvolve") {
        var result = [Double](repeating: 0,
                              count: count)
        vDSP.convolve(signal,
                      withKernel: kernel,
                      result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_convD(signal, 1,
                   UnsafePointer(kernel).advanced(by: kernel.count - 1), -1,
                   &legacyResult, 1,
                   n,
                   vDSP_Length(kernel.count))
        
        let returnedResult = vDSP.convolve(signal,
                                           withKernel: kernel)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/DoublePrecisionCorrelate") {
        var result = [Double](repeating: 0,
                              count: count)
        vDSP.correlate(signal,
                       withKernel: kernel,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_convD(signal, 1,
                   kernel, 1,
                   &legacyResult, 1,
                   n,
                   vDSP_Length(kernel.count))
        
        let returnedResult = vDSP.correlate(signal,
                                            withKernel: kernel)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Single-precision 2D convolution
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let width = 512
    let height = 128
    let pixelCount = 512 * 128
    
    let pixels: [Float] = (0 ..< pixelCount).map { i in
        let x = floor(Float(i) / 1024) * 25
        let y = remainder(Float(i), 1024) * 25
        return (sin(y).sign == .minus && sin(x).sign == .minus) ? 0 : 1
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/SinglePrecision3x3") {
        let kernel3x3 = [Float](repeating: 1.0 / 9, count: 9)
        
        var result = [Float](repeating: .nan,
                             count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      with3x3Kernel: kernel3x3,
                      result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: result.count)
        
        vDSP_f3x3(pixels,
                  vDSP_Length(height), vDSP_Length(width),
                  kernel3x3,
                  &legacyResult)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           with3x3Kernel: kernel3x3)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/SinglePrecision5x5") {
        let kernel5x5 = [Float](repeating: 1.0 / 25, count: 25)
        
        var result = [Float](repeating: .nan,
                             count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      with5x5Kernel: kernel5x5,
                      result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: result.count)
        
        vDSP_f5x5(pixels,
                  vDSP_Length(height), vDSP_Length(width),
                  kernel5x5,
                  &legacyResult)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           with5x5Kernel: kernel5x5)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/SinglePrecision7x3") {
        let kernel7x3 = [Float](repeating: 1.0 / (7 * 3), count: 7 * 3)
        
        var result = [Float](repeating: .nan,
                             count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      withKernel: kernel7x3,
                      kernelRowCount: 7, kernelColumnCount: 3,
                      result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: result.count)
        
        vDSP_imgfir(pixels,
                    vDSP_Length(height), vDSP_Length(width),
                    kernel7x3,
                    &legacyResult,
                    7, 3)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           withKernel: kernel7x3,
                                           kernelRowCount: 7, kernelColumnCount: 3)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Double-precision 2D convolution
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let width = 512
    let height = 128
    let pixelCount = 512 * 128
    
    let pixels: [Double] = (0 ..< pixelCount).map { i in
        let x = floor(Double(i) / 1024) * 25
        let y = remainder(Double(i), 1024) * 25
        return (sin(y).sign == .minus && sin(x).sign == .minus) ? 0 : 1
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/DoublePrecision3x3") {
        let kernel3x3 = [Double](repeating: 1.0 / 9, count: 9)
        
        var result = [Double](repeating: .nan,
                              count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      with3x3Kernel: kernel3x3,
                      result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: result.count)
        
        vDSP_f3x3D(pixels,
                   vDSP_Length(height), vDSP_Length(width),
                   kernel3x3,
                   &legacyResult)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           with3x3Kernel: kernel3x3)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/DoublePrecision5x5") {
        let kernel5x5 = [Double](repeating: 1.0 / 25, count: 25)
        
        var result = [Double](repeating: .nan,
                              count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      with5x5Kernel: kernel5x5,
                      result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: result.count)
        
        vDSP_f5x5D(pixels,
                   vDSP_Length(height), vDSP_Length(width),
                   kernel5x5,
                   &legacyResult)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           with5x5Kernel: kernel5x5)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vDSPConvolutionTests.test("vDSP/DoublePrecision7x3") {
        let kernel7x3 = [Double](repeating: 1.0 / (7 * 3), count: 7 * 3)
        
        var result = [Double](repeating: .nan,
                              count: pixelCount)
        
        vDSP.convolve(pixels,
                      rowCount: height, columnCount: width,
                      withKernel: kernel7x3,
                      kernelRowCount: 7, kernelColumnCount: 3,
                      result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: result.count)
        
        vDSP_imgfirD(pixels,
                     vDSP_Length(height), vDSP_Length(width),
                     kernel7x3,
                     &legacyResult,
                     7, 3)
        
        let returnedResult = vDSP.convolve(pixels,
                                           rowCount: height, columnCount: width,
                                           withKernel: kernel7x3,
                                           kernelRowCount: 7, kernelColumnCount: 3)
            
            expectTrue(result.elementsEqual(legacyResult))
            expectTrue(result.elementsEqual(returnedResult))
    }
}

runAllTests()
