// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
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

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
        
        expectTrue(returnedResult.elementsEqual(legacyResult))
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
        
        expectTrue(returnedResult.elementsEqual(legacyResult))
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
        
        expectTrue(returnedResult.elementsEqual(legacyResult))
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
        
        expectTrue(returnedResult.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP type-conversion tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    let n = vDSP_Length(10)
    
    let floatingPointValues: [Float] = [-10.9, -20.1, 32.99, 14.001, -16.5,
                                        90.5, 0, -0, -0.00001, 0.000001]
    
    let floatingPointValuesD: [Double] = [-10.9, -20.1, 32.99, 14.001, -16.5,
                                          90.5, 0, -0, -0.00001, 0.000001]
    
    let unsignedIntValues = [5, 250, 0, 99, 127, 65, 78, 1, 33, 10]
    let signedIntValues = [5, -126, 0, -99, 125, -65, 78, -1, 33, -10]
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToDouble") {
        let source = floatingPointValues
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vspdp(source, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.floatToDouble(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToFloat") {
        let source = floatingPointValuesD
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vdpsp(source, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.doubleToFloat(source)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/UInt8_to_Float") {
        let source = unsignedIntValues.map{ return UInt8($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu8(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/UInt8_to_Double") {
        let source = unsignedIntValues.map{ return UInt8($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)

        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/UInt16_to_Float") {
        let source = unsignedIntValues.map{ return UInt16($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu16(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/UInt16_to_Double") {
        let source = unsignedIntValues.map{ return UInt16($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)

        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }

    AccelerateTests_vDSPConversion.test("vDSP/UInt32_to_Float") {
        let source = unsignedIntValues.map{ return UInt32($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu32(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/UInt32_to_Double") {
        let source = unsignedIntValues.map{ return UInt32($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vfltu32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)

        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int8_to_Float") {
        let source = signedIntValues.map{ return Int8($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt8(source, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int8_to_Double") {
        let source = signedIntValues.map{ return Int8($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt8D(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int16_to_Float") {
        let source = signedIntValues.map{ return Int16($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt16(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int16_to_Double") {
        let source = signedIntValues.map{ return Int16($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt16D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int32_to_Float") {
        let source = signedIntValues.map{ return Int32($0) }
        var result = [Float](repeating: 0, count: source.count)
        var legacyResult = [Float](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt32(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Float.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/Int32_to_Double") {
        let source = signedIntValues.map{ return Int32($0) }
        var result = [Double](repeating: 0, count: source.count)
        var legacyResult = [Double](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result)
        
        vDSP_vflt32D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.integerToFloatingPoint(source,
                                                         floatingPointType: Double.self)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt32towardZero") {
        let source = floatingPointValues
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix32(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int32.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt32towardNearest") {
        let source = floatingPointValues
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr32(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int32.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt32towardZero") {
        let source = floatingPointValuesD
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix32D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int32.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt32towardNearest") {
        let source = floatingPointValuesD
        var result = [Int32](repeating: 0, count: source.count)
        var legacyResult = [Int32](repeating: -1, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int32.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt16towardZero") {
        let source = floatingPointValues
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu16(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt16.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt16towardNearest") {
        let source = floatingPointValues
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru16(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt16.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToUInt16towardZero") {
        let source = floatingPointValuesD
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt16.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToUInt16towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt16](repeating: 0, count: source.count)
        var legacyResult = [UInt16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru16D(source, 1,
                       &legacyResult, 1,
                       n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt16.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt32towardZero") {
        let source = floatingPointValues
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu32(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt32.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt32towardNearest") {
        let source = floatingPointValues
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru32(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt32.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToUInt32towardZero") {
        let source = floatingPointValuesD
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu32D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt32.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToUInt32towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt32](repeating: 0, count: source.count)
        var legacyResult = [UInt32](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru32D(source, 1,
                       &legacyResult, 1,
                       n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt32.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt16towardZero") {
        let source = floatingPointValues
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix16(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int16.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt16towardNearest") {
        let source = floatingPointValues
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr16(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int16.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt16towardZero") {
        let source = floatingPointValuesD
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix16D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int16.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt16towardNearest") {
        let source = floatingPointValuesD
        var result = [Int16](repeating: 0, count: source.count)
        var legacyResult = [Int16](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr16D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int16.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt8towardZero") {
        let source = floatingPointValues
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix8(source, 1,
                   &legacyResult, 1,
                   n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int8.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToInt8towardNearest") {
        let source = floatingPointValues
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr8(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int8.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt8towardZero") {
        let source = floatingPointValuesD
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfix8D(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int8.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToInt8towardNearest") {
        let source = floatingPointValuesD
        var result = [Int8](repeating: 0, count: source.count)
        var legacyResult = [Int8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixr8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: Int8.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt8towardZero") {
        let source = floatingPointValues
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu8(source, 1,
                    &legacyResult, 1,
                    n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt8.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/FloatToUInt8towardNearest") {
        let source = floatingPointValues
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru8(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt8.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubleToUInt8towardZero") {
        let source = floatingPointValuesD
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardZero)
        
        vDSP_vfixu8D(source, 1,
                     &legacyResult, 1,
                     n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt8.self,
                                                         rounding: .towardZero)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    AccelerateTests_vDSPConversion.test("vDSP/DoubletoUInt8towardNearest") {
        let source = floatingPointValuesD
        var result = [UInt8](repeating: 0, count: source.count)
        var legacyResult = [UInt8](repeating: 99, count: source.count)
        
        vDSP.convertElements(of: source, to: &result, rounding: .towardNearestInteger)
        
        vDSP_vfixru8D(source, 1,
                      &legacyResult, 1,
                      n)
        
        let returnedResult = vDSP.floatingPointToInteger(source,
                                                         integerType: UInt8.self,
                                                         rounding: .towardNearestInteger)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP decibel conversion
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP complex format conversion
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
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
