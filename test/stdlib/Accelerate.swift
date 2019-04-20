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
//  vDSP Discrete Cosine Transform
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *) {
    
    AccelerateTests.test("vDSP/DiscreteCosineTransform") {
        let n = 1024
        
        let source = (0 ..< n).map{ i in
            return sin(Float(i) * 0.05) + sin(Float(i) * 0.025)
        }
        
        for transformType in vDSP.DCTTransformType.allCases {
            
            let dct = vDSP.DCT(count: n,
                               transformType: transformType)
            
            var destination = [Float](repeating: 0,
                                      count: n)
            
            dct?.transform(source,
                           result: &destination)
            
            let returnedResult = dct!.transform(source)
            
            // Legacy API
            
            let legacySetup = vDSP_DCT_CreateSetup(nil,
                                                   vDSP_Length(n),
                                                   transformType.dctType)!
            
            var legacyDestination = [Float](repeating: -1,
                                            count: n)
            
            vDSP_DCT_Execute(legacySetup,
                             source,
                             &legacyDestination)
            
            expectTrue(destination.elementsEqual(legacyDestination))
            expectTrue(destination.elementsEqual(returnedResult))
        }
    }
}

runAllTests()
