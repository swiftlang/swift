
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
//  vDSP Single-precision 1D convolution and correlation
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    
    let kernel:  [Float] = ( 0 ..< 10 ).map {
        return Float($0) / 45
    }
    
    let signal: [Float] = (0 ..< 256 + 10).map {
        return sin(Float($0) * 0.05).sign == .minus ? -1 : 1
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionConvolve") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecisionCorrelate") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Double-precision 1D convolution and correlation
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 256
    let n = vDSP_Length(256)
    
    let kernel:  [Double] = ( 0 ..< 10 ).map {
        return Double($0) / 45
    }
    
    let signal: [Double] = (0 ..< 256 + 10).map {
        return sin(Double($0) * 0.05).sign == .minus ? -1 : 1
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionConvolve") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecisionCorrelate") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Single-precision 2D convolution
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let width = 512
    let height = 128
    let pixelCount = 512 * 128
    
    let pixels: [Float] = (0 ..< pixelCount).map { i in
        let x = floor(Float(i) / 1024) * 25
        let y = remainder(Float(i), 1024) * 25
        return (sin(y).sign == .minus && sin(x).sign == .minus) ? 0 : 1
    }
    
    AccelerateTests.test("vDSP/SinglePrecision3x3") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecision5x5") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/SinglePrecision5x5") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Double-precision 2D convolution
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let width = 512
    let height = 128
    let pixelCount = 512 * 128
    
    let pixels: [Double] = (0 ..< pixelCount).map { i in
        let x = floor(Double(i) / 1024) * 25
        let y = remainder(Double(i), 1024) * 25
        return (sin(y).sign == .minus && sin(x).sign == .minus) ? 0 : 1
    }
    
    AccelerateTests.test("vDSP/DoublePrecision3x3") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecision5x5") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoublePrecision5x5") {
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
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

runAllTests()
