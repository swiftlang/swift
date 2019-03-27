// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vDSPIntegrationTests = TestSuite("Accelerate_vDSPIntegration")

if #available(iOS 10.0, OSX 10.12, tvOS 10.0, watchOS 4.0, *) {
  
  Accelerate_vDSPIntegrationTests.test("BNNS/ImageStackDescriptor") {
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
  
  Accelerate_vDSPIntegrationTests.test("BNNS/VectorDescriptor") {
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
  
  Accelerate_vDSPIntegrationTests.test("BNNS/LayerData") {
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
  
  Accelerate_vDSPIntegrationTests.test("BNNS/Activation") {
    expectEqual(BNNSActivation.identity.function, .identity)
    let id = BNNSActivation(function: .identity)
    expectTrue(id.alpha.isNaN)
    expectTrue(id.beta.isNaN)
  }
  
}

//===----------------------------------------------------------------------===//
//
//  vDSP integration tests
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    let count = 1024
    let n = vDSP_Length(1024)
    
    let sourcef: [Float] = (0 ..< 1024).map {
        return sin(Float($0) * 0.03) * cos(Float($0) * 0.07)
    }
    
    let sourced: [Double] = (0 ..< 1024).map {
        return sin(Double($0) * 0.03) * cos(Double($0) * 0.07)
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/SinglePrecisionRunningSum") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.integrate(sourcef,
                       using: .runningSum,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vrsum(sourcef, 1,
                   [1],
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/SinglePrecisionTrapezoidal") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.integrate(sourcef,
                       using: .simpson,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vsimps(sourcef, 1,
                    [1],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/SinglePrecisionTrapezoidal") {
        var result = [Float](repeating: 0,
                             count: count)
        
        vDSP.integrate(sourcef,
                       using: .trapezoidal,
                       result: &result)
        
        var legacyResult = [Float](repeating: -1,
                                   count: count)
        
        vDSP_vtrapz(sourcef, 1,
                    [1],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/DoublePrecisionRunningSum") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.integrate(sourced,
                       using: .runningSum,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vrsumD(sourced, 1,
                    [1],
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/DoublePrecisionSimpson") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.integrate(sourced,
                       using: .simpson,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vsimpsD(sourced, 1,
                     [1],
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    Accelerate_vDSPIntegrationTests.test("vDSP/DoublePrecisionTrapezoidal") {
        var result = [Double](repeating: 0,
                              count: count)
        
        vDSP.integrate(sourced,
                       using: .trapezoidal,
                       result: &result)
        
        var legacyResult = [Double](repeating: -1,
                                    count: count)
        
        vDSP_vtrapzD(sourced, 1,
                     [1],
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

runAllTests()
