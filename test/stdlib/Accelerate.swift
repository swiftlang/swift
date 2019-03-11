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
//  vDSP type-conversion tests
//
//===----------------------------------------------------------------------===//

if #available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *) {
    
    let n = vDSP_Length(10)
    
    let floatingPointValues: [Float] = [-10.9, -20.1, 32.99, 14.001, -16.5,
                                        90.5, 0, -0, -0.00001, 0.000001]
    
    let floatingPointValuesD: [Double] = [-10.9, -20.1, 32.99, 14.001, -16.5,
                                          90.5, 0, -0, -0.00001, 0.000001]
    
    let unsignedIntValues = [5, 250, 0, 99, 127, 65, 78, 1, 33, 10]
    let signedIntValues = [5, -126, 0, -99, 125, -65, 78, -1, 33, -10]
    
    AccelerateTests.test("vDSP/FloatToDouble") {
        let source = floatingPointValues
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vspdp(source, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToDouble") {
        let source = floatingPointValuesD
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vdpsp(source, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt8_to_Float") {
        let source = unsignedIntValues.map{ return UInt8($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu8(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt8_to_Double") {
        let source = unsignedIntValues.map{ return UInt8($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt16_to_Float") {
        let source = unsignedIntValues.map{ return UInt16($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu16(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt16_to_Double") {
        let source = unsignedIntValues.map{ return UInt16($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt32_to_Float") {
        let source = unsignedIntValues.map{ return UInt32($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu32(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/UInt32_to_Double") {
        let source = unsignedIntValues.map{ return UInt32($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vfltu32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int8_to_Float") {
        let source = signedIntValues.map{ return Int8($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt8(source, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int8_to_Double") {
        let source = signedIntValues.map{ return Int8($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt8D(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int16_to_Float") {
        let source = signedIntValues.map{ return Int16($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt16(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int16_to_Double") {
        let source = signedIntValues.map{ return Int16($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt16D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int32_to_Float") {
        let source = signedIntValues.map{ return Int32($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt32(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/Int32_to_Double") {
        let source = signedIntValues.map{ return Int32($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result)
        
        vDSP_vflt32D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt32towardZero") {
        let source = floatingPointValues
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix32(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt32towardNearest") {
        let source = floatingPointValues
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr32(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt32towardZero") {
        let source = floatingPointValuesD
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix32D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt32towardNearest") {
        let source = floatingPointValuesD
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt16towardZero") {
        let source = floatingPointValues
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu16(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt16towardNearest") {
        let source = floatingPointValues
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru16(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToUInt16towardZero") {
        let source = floatingPointValuesD
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToUInt16towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru16D(source, 1,
                       &legacyResult, 1,
                       n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt32towardZero") {
        let source = floatingPointValues
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu32(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt32towardNearest") {
        let source = floatingPointValues
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru32(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToUInt32towardZero") {
        let source = floatingPointValuesD
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToUInt32towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru32D(source, 1,
                       &legacyResult, 1,
                       n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt16towardZero") {
        let source = floatingPointValues
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix16(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt16towardNearest") {
        let source = floatingPointValues
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr16(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt16towardZero") {
        let source = floatingPointValuesD
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix16D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt16towardNearest") {
        let source = floatingPointValuesD
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt8towardZero") {
        let source = floatingPointValues
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix8(source, 1,
                   &legacyResult, 1,
                   n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToInt8towardNearest") {
        let source = floatingPointValues
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr8(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt8towardZero") {
        let source = floatingPointValuesD
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfix8D(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToInt8towardNearest") {
        let source = floatingPointValuesD
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt8towardZero") {
        let source = floatingPointValues
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu8(source, 1,
                    &legacyResult, 1,
                    n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/FloatToUInt8towardNearest") {
        let source = floatingPointValues
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru8(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubleToUInt8towardZero") {
        let source = floatingPointValuesD
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vDSP/DoubletoUInt8towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convert(source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru8D(source, 1,
                      &legacyResult, 1,
                      n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

runAllTests()
