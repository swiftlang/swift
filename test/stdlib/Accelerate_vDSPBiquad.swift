// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPBiquadTests = TestSuite("Accelerate_vDSPBiquad")

//===----------------------------------------------------------------------===//
//
//  vDSP Biquad Filters
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let n = 256
    
    // low pass
    let b0 = 0.0001
    let b1 = 0.001
    let b2 = 0.0005
    let a1 = -1.9795
    let a2 = 0.98
    
    let sections = vDSP_Length(1)
    
    Accelerate_vDSPBiquadTests.test("vDSP/BiquadNil") {
        let biquad = vDSP.Biquad(coefficients: [0.0],
                                 channelCount: 9999,
                                 sectionCount: 9999,
                                 ofType: Float.self)
        
        expectTrue(biquad == nil)
    }
    
    Accelerate_vDSPBiquadTests.test("vDSP/SingleChannelSinglePrecision") {
        let channels = vDSP_Length(1)
        
        let signal: [Float] = (0 ..< n).map { i in
            return sin(Float(i) * 0.1) + sin(Float(i) * 0.01)
        }
        
        var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                 channelCount: sections,
                                 sectionCount: channels,
                                 ofType: Float.self)!
        
        var returningBiquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                          channelCount: sections,
                                          sectionCount: channels,
                                          ofType: Float.self)!
        
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
            
            let returnedOutput = returningBiquad.apply(input: signal)
            
            vDSP_biquad(setup,
                        &delays,
                        signal, 1,
                        &legacyOutput, 1,
                        vDSP_Length(n))
            
            expectTrue(output.elementsEqual(legacyOutput))
            expectTrue(output.elementsEqual(returnedOutput))
        }
    }
    
    Accelerate_vDSPBiquadTests.test("vDSP/MultiChannelSinglePrecision") {
        let channelCount = vDSP_Length(2)
        
        let signal: [Float] = (0 ..< n).map { i in
            return sin(Float(i) * 0.1) + sin(Float(i) * 0.01)
        }
        
        var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                b0, b1, b2, a1, a2],
                                 channelCount: channelCount,
                                 sectionCount: sections,
                                 ofType: Float.self)!
        
        var returningBiquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                         b0, b1, b2, a1, a2],
                                          channelCount: channelCount,
                                          sectionCount: sections,
                                          ofType: Float.self)!
        
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
            
            let returnedOutput = returningBiquad.apply(input: signal)
            
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
            expectTrue(output.elementsEqual(returnedOutput))
        }
    }
    
    Accelerate_vDSPBiquadTests.test("vDSP/SingleChannelDoublePrecision") {
        let channels = vDSP_Length(1)
        
        let signal: [Double] = (0 ..< n).map { i in
            return sin(Double(i) * 0.1) + sin(Double(i) * 0.01)
        }
        
        var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                 channelCount: sections,
                                 sectionCount: channels,
                                 ofType: Double.self)!
        
        var returningBiquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2],
                                          channelCount: sections,
                                          sectionCount: channels,
                                          ofType: Double.self)!
        
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
            
            let returnedOutput = returningBiquad.apply(input: signal)
            
            vDSP_biquadD(setup,
                         &delays,
                         signal, 1,
                         &legacyOutput, 1,
                         vDSP_Length(n))
            
            expectTrue(output.elementsEqual(legacyOutput))
            expectTrue(output.elementsEqual(returnedOutput))
        }
    }
    
    Accelerate_vDSPBiquadTests.test("vDSP/MultiChannelDoublePrecision") {
        let channelCount = vDSP_Length(2)
        
        let signal: [Double] = (0 ..< n).map { i in
            return sin(Double(i) * 0.1) + sin(Double(i) * 0.01)
        }
        
        var biquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                b0, b1, b2, a1, a2],
                                 channelCount: channelCount,
                                 sectionCount: sections,
                                 ofType: Double.self)!
        
        var returningBiquad = vDSP.Biquad(coefficients: [b0, b1, b2, a1, a2,
                                                         b0, b1, b2, a1, a2],
                                          channelCount: channelCount,
                                          sectionCount: sections,
                                          ofType: Double.self)!
        
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
            
            let returnedOutput = returningBiquad.apply(input: signal)
            
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
            expectTrue(output.elementsEqual(returnedOutput))
        }
    }
}

runAllTests()
