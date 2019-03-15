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
//  vDSP Single Precision Arithmetic
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = 256
    
    // low pass
    let b0 = 0.0001
    let b1 = 0.001
    let b2 = 0.0005
    let a1 = -1.9795
    let a2 = 0.98
    
    let sections = vDSP_Length(1)
    
    AccelerateTests.test("vDSP/BiquadNil") {
        let biquad = vDSP.Biquad(coefficients: [0.0],
                                 channelCount: 9999,
                                 sectionCount: 9999,
                                 ofType: Float.self)
        
        expectTrue(biquad == nil)
    }
    
    AccelerateTests.test("vDSP/SingleChannelSinglePrecision") {
        let channels = vDSP_Length(1)
        
        let signal: [Float] = (0 ..< n).map { i in
            return sin(Float(i) * 0.1) + sin(Float(i) * 0.01)
        }
        
        guard var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                       channelCount: sections,
                                       sectionCount: channels,
                                       ofType: Float.self) else {
                                        expectationFailure("Failed to create single-precision Biquad.",
                                                           trace: "",
                                                           stackTrace: SourceLocStack())
                                        return
        }
        
        var output = [Float](repeating: 0,
                             count: n)
        
        // Legacy...
        var legacyOutput = [Float](repeating: -1,
                                   count: n)
        
        var delays = [Float](repeating: 0.0,
                             count: Int(2 * sections) + 2)
        
        let setup = vDSP_biquad_CreateSetup([b0, b1, b2, a1, a2],
                                            sections)!
        
        for _ in 0 ... 3 {
            
            biquad.apply(input: signal,
                         output: &output)
            
            vDSP_biquad(setup,
                        &delays,
                        signal, 1,
                        &legacyOutput, 1,
                        vDSP_Length(n))
            
            expectTrue(output.elementsEqual(legacyOutput))
        }
    }
    
    AccelerateTests.test("vDSP/MultiChannelSinglePrecision") {
        let channelCount = vDSP_Length(2)
        
        let signal: [Float] = (0 ..< n).map { i in
            return sin(Float(i) * 0.1) + sin(Float(i) * 0.01)
        }
        
        guard var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                      b0, b1, b2, a1, a2],
                                       channelCount: channelCount,
                                       sectionCount: sections,
                                       ofType: Float.self) else {
                                        expectationFailure("Failed to create single-precision Biquad (multi).",
                                                           trace: "",
                                                           stackTrace: SourceLocStack())
                                        return
        }
        
        var output = [Float](repeating: 0,
                             count: n)
        
        // Legacy...
        var legacyOutput = [Float](repeating: -1,
                                   count: n)
        
        let setup = vDSP_biquadm_CreateSetup([b0, b1, b2, a1, a2,
                                              b0, b1, b2, a1, a2],
                                             vDSP_Length(sections),
                                             vDSP_Length(channelCount))!
        
        for _ in 0 ... 3 {
            
            biquad.apply(input: signal,
                         output: &output)
            
            signal.withUnsafeBufferPointer { inputBuffer in
                let inputPointer = inputBuffer.baseAddress!
                legacyOutput.withUnsafeMutableBufferPointer { outputBuffer in
                    let outputPointer = outputBuffer.baseAddress!
                    
                    let length = vDSP_Length(n) / channelCount
                    
                    var inputs: [UnsafePointer<Float>] = (0 ..< channelCount).map { i in
                        return inputPointer.advanced(by: Int(i * length))
                    }
                    
                    var outputs: [UnsafeMutablePointer<Float>] = (0 ..< channelCount).map { i in
                        return outputPointer.advanced(by: Int(i * length))
                    }
                    
                    vDSP_biquadm(setup, &inputs, 1, &outputs, 1, vDSP_Length(n) / channelCount)
                }
            }
            
            expectTrue(output.elementsEqual(legacyOutput))
        }
    }
    
    AccelerateTests.test("vDSP/SingleChannelDoublePrecision") {
        let channels = vDSP_Length(1)
        
        let signal: [Double] = (0 ..< n).map { i in
            return sin(Double(i) * 0.1) + sin(Double(i) * 0.01)
        }
        
        guard var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                       channelCount: sections,
                                       sectionCount: channels,
                                       ofType: Double.self) else {
                                        expectationFailure("Failed to create double-precision Biquad.",
                                                           trace: "",
                                                           stackTrace: SourceLocStack())
                                        return
        }
        
        var output = [Double](repeating: 0,
                              count: n)
        
        // Legacy...
        var legacyOutput = [Double](repeating: -1,
                                    count: n)
        
        var delays = [Double](repeating: 0.0,
                              count: Int(2 * sections) + 2)
        
        let setup = vDSP_biquad_CreateSetupD([b0, b1, b2, a1, a2],
                                             sections)!
        
        for _ in 0 ... 3 {
            
            biquad.apply(input: signal,
                         output: &output)
            
            vDSP_biquadD(setup,
                         &delays,
                         signal, 1,
                         &legacyOutput, 1,
                         vDSP_Length(n))
            
            expectTrue(output.elementsEqual(legacyOutput))
        }
    }
    
    AccelerateTests.test("vDSP/MultiChannelDoublePrecision") {
        let channelCount = vDSP_Length(2)
        
        let signal: [Double] = (0 ..< n).map { i in
            return sin(Double(i) * 0.1) + sin(Double(i) * 0.01)
        }
        
        guard var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                      b0, b1, b2, a1, a2],
                                       channelCount: channelCount,
                                       sectionCount: sections,
                                       ofType: Double.self) else {
                                        expectationFailure("Failed to create double-precision Biquad (multi).",
                                                           trace: "",
                                                           stackTrace: SourceLocStack())
                                        return
        }
        
        var output = [Double](repeating: 0,
                              count: n)
        
        // Legacy...
        var legacyOutput = [Double](repeating: -1,
                                    count: n)
        
        let setup = vDSP_biquadm_CreateSetupD([b0, b1, b2, a1, a2,
                                               b0, b1, b2, a1, a2],
                                              vDSP_Length(sections),
                                              vDSP_Length(channelCount))!
        
        for _ in 0 ... 3 {
            
            biquad.apply(input: signal,
                         output: &output)
            
            signal.withUnsafeBufferPointer { inputBuffer in
                let inputPointer = inputBuffer.baseAddress!
                legacyOutput.withUnsafeMutableBufferPointer { outputBuffer in
                    let outputPointer = outputBuffer.baseAddress!
                    
                    let length = vDSP_Length(n) / channelCount
                    
                    var inputs: [UnsafePointer<Double>] = (0 ..< channelCount).map { i in
                        return inputPointer.advanced(by: Int(i * length))
                    }
                    
                    var outputs: [UnsafeMutablePointer<Double>] = (0 ..< channelCount).map { i in
                        return outputPointer.advanced(by: Int(i * length))
                    }
                    
                    vDSP_biquadmD(setup, &inputs, 1, &outputs, 1, vDSP_Length(n) / channelCount)
                }
            }
            
            expectTrue(output.elementsEqual(legacyOutput))
        }
    }
}

runAllTests()
