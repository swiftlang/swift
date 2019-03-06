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
//  vForce single-precision tests
//
//===----------------------------------------------------------------------===//

if #available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Float]()
    var legacyResult = [Float]()
    let x: [Float] = [2, 4, -6.876, 10.9, -100.3, 100]
    let y: [Float] = [-10, -20, 40, 60, -80, -300]
    let z: [Float] = [-0.6, 0.2, -0.1, 0.9, -0.1, 0.1]
    let count = 6
    var n: Int32 = 6
    
    AccelerateTests.test("vForce/SinglePrecisionCeiling") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.ceiling(x,
                     result: &result)
        
        vvceilf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionFloor") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.floor(x,
                   result: &result)
        
        vvfloorf(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionCopySign") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.copySign(magnitudes: x,
                      signs: y,
                      result: &result)
        
        vvcopysignf(&legacyResult,
                    x,
                    y,
                    &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionModulus") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.modulus(numerators: x,
                     denominators: y,
                     result: &result)
        
        vvfmodf(&legacyResult,
                x,
                y,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionModulusParameters") {
        var scalarResult = [Float.nan]
        let scalarNumerator = [Float(20)]
        let scalarDenominator = [Float(6)]
        
        vDSP.modulus(numerators: scalarNumerator,
                     denominators: scalarDenominator,
                     result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    AccelerateTests.test("vForce/SinglePrecisionRemainder") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.remainder(numerators: x,
                       denominators: y,
                       result: &result)
        
        vvremainderf(&legacyResult,
                     x,
                     y,
                     &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionRemainderParameters") {
        var scalarResult = [Float.nan]
        let scalarNumerator = [Float(20)]
        let scalarDenominator = [Float(6)]
        
        vDSP.remainder(numerators: scalarNumerator,
                       denominators: scalarDenominator,
                       result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    AccelerateTests.test("vForce/SinglePrecisionIntegerTruncation") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.integerTruncation(x,
                               result: &result)
        
        vvintf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionNearestInteger") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.nearestInteger(x,
                            result: &result)
        
        vvnintf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionReciprocalSquareRoot") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.reciprocalSquareRoot(x,
                                  result: &result)
        
        vvrsqrtf(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionSquareRoot") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.squareRoot(x,
                        result: &result)
        
        vvsqrtf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionReciprocal") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.reciprocal(x,
                        result: &result)
        
        vvrecf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionExp") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .exp,
                         result: &result)
        
        vvexpf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionExpm1") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .expm1,
                         result: &result)
        
        vvexpm1f(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionExp2") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .exp2,
                         result: &result)
        
        vvexp2f(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionLog2") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.logarithm(x,
                       base: .two,
                       result: &result)
        
        vvlog2f(&legacyResult,
                x,
                &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionLog10") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.logarithm(x,
                       base: .ten,
                       result: &result)
        
        vvlog10f(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionUnbiasedExponent") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.unbiasedExponent(x,
                              result: &result)
        
        vvlogbf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionPower") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.power(bases: x,
                   exponents: y,
                   result: &result)
        
        vvpowf(&legacyResult,
               y,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionPower") {
        var scalarResult = [Float.nan]
        let scalarBase = [Float(10)]
        let scalarExponent = [Float(2)]
        
        vDSP.power(bases: scalarBase,
                   exponents: scalarExponent,
                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 100)
    }
    
    AccelerateTests.test("vForce/SinglePrecisionSine") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.sin(x,
                 result: &result)
        
        vvsinf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionSinPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.sinPi(x,
                   result: &result)
        
        vvsinpif(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionCosine") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.cos(x,
                 result: &result)
        
        vvcosf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionCosPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.cosPi(x,
                   result: &result)
        
        vvcospif(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionSinCos") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        var cosResult = [Float](repeating: .nan, count: result.count)
        var legacyCosResult = [Float](repeating: .nan, count: result.count)
        
        vDSP.sinCos(x,
                    sinResult: &result,
                    cosResult: &cosResult)
        
        vvsincosf(&legacyResult,
                  &legacyCosResult,
                  x,
                  &n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(cosResult.elementsEqual(legacyCosResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionTangent") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.tan(x,
                 result: &result)
        
        vvtanf(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionTanPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.tanPi(x,
                   result: &result)
        
        vvtanpif(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionArcsin") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.arcsin(z,
                    result: &result)
        
        vvasinf(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionArccos") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.arccos(z,
                    result: &result)
        
        vvacosf(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionArctan") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.arctan(z,
                    result: &result)
        
        vvatanf(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionSinh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.sinh(x,
                  result: &result)
        
        vvsinhf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionCosh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.cosh(x,
                  result: &result)
        
        vvcoshf(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionTanh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.tanh(z,
                  result: &result)
        
        vvtanhf(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionAsinh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.asinh(z,
                   result: &result)
        
        vvasinhf(&legacyResult,
                 z,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionAcosh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.acosh(x,
                   result: &result)
        
        vvacoshf(&legacyResult,
                 x,
                 &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/SinglePrecisionAtan") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vDSP.atanh(z,
                   result: &result)
        
        vvatanhf(&legacyResult,
                 z,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vForce double precision tests
//
//===----------------------------------------------------------------------===//

if #available(iOS 13.0, macOS 10.14, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Double]()
    var legacyResult = [Double]()
    let x: [Double] = [2, 4, -6.876, 10.9, -100.3, 100]
    let y: [Double] = [-10, -20, 40, 60, -80, -300]
    let z: [Double] = [-0.6, 0.2, -0.1, 0.9, -0.1, 0.1]
    let count = 6
    var n: Int32 = 6
    
    AccelerateTests.test("vForce/DoublePrecisionCeiling") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.ceiling(x,
                     result: &result)
        
        vvceil(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionFloor") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.floor(x,
                   result: &result)
        
        vvfloor(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionCopySign") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.copySign(magnitudes: x,
                      signs: y,
                      result: &result)
        
        vvcopysign(&legacyResult,
                   x,
                   y,
                   &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionModulus") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.modulus(numerators: x,
                     denominators: y,
                     result: &result)
        
        vvfmod(&legacyResult,
               x,
               y,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionModulusParameters") {
        var scalarResult = [Double.nan]
        let scalarNumerator = [Double(20)]
        let scalarDenominator = [Double(6)]
        
        vDSP.modulus(numerators: scalarNumerator,
                     denominators: scalarDenominator,
                     result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    AccelerateTests.test("vForce/DoublePrecisionRemainder") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.remainder(numerators: x,
                       denominators: y,
                       result: &result)
        
        vvremainder(&legacyResult,
                    x,
                    y,
                    &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionRemainderParameters") {
        var scalarResult = [Double.nan]
        let scalarNumerator = [Double(20)]
        let scalarDenominator = [Double(6)]
        
        vDSP.remainder(numerators: scalarNumerator,
                       denominators: scalarDenominator,
                       result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    AccelerateTests.test("vForce/DoublePrecisionIntegerTruncation") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.integerTruncation(x,
                               result: &result)
        
        vvint(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionNearestInteger") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.nearestInteger(x,
                            result: &result)
        
        vvnint(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionReciprocalSquareRoot") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.reciprocalSquareRoot(x,
                                  result: &result)
        
        vvrsqrt(&legacyResult,
                x,
                &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionSquareRoot") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.squareRoot(x,
                        result: &result)
        
        vvsqrt(&legacyResult,
               x,
               &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionReciprocal") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.reciprocal(x,
                        result: &result)
        
        vvrec(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionExp") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .exp,
                         result: &result)
        
        vvexp(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionExpm1") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .expm1,
                         result: &result)
        
        vvexpm1(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionExp2") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.exponential(x,
                         exponentialType: .exp2,
                         result: &result)
        
        vvexp2(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionLog2") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.logarithm(x,
                       base: .two,
                       result: &result)
        
        vvlog2(&legacyResult,
               x,
               &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionLog10") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.logarithm(x,
                       base: .ten,
                       result: &result)
        
        vvlog10(&legacyResult,
                x,
                &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionUnbiasedExponent") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.unbiasedExponent(x,
                              result: &result)
        
        vvlogb(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionPower") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.power(bases: x,
                   exponents: y,
                   result: &result)
        
        vvpow(&legacyResult,
              y,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionPower") {
        var scalarResult = [Double.nan]
        let scalarBase = [Double(10)]
        let scalarExponent = [Double(2)]
        
        vDSP.power(bases: scalarBase,
                   exponents: scalarExponent,
                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 100)
    }
    
    AccelerateTests.test("vForce/DoublePrecisionSine") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.sin(x,
                 result: &result)
        
        vvsin(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionSinPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.sinPi(x,
                   result: &result)
        
        vvsinpi(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionCosine") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.cos(x,
                 result: &result)
        
        vvcos(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionCosPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.cosPi(x,
                   result: &result)
        
        vvcospi(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionSinCos") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        var cosResult = [Double](repeating: .nan, count: result.count)
        var legacyCosResult = [Double](repeating: .nan, count: result.count)
        
        vDSP.sinCos(x,
                    sinResult: &result,
                    cosResult: &cosResult)
        
        vvsincos(&legacyResult,
                 &legacyCosResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(cosResult.elementsEqual(legacyCosResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionTangent") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.tan(x,
                 result: &result)
        
        vvtan(&legacyResult,
              x,
              &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionTanPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.tanPi(x,
                   result: &result)
        
        vvtanpi(&legacyResult,
                x,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionArcsin") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.arcsin(z,
                    result: &result)
        
        vvasin(&legacyResult,
               z,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionArccos") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.arccos(z,
                    result: &result)
        
        vvacos(&legacyResult,
               z,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionArctan") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.arctan(z,
                    result: &result)
        
        vvatan(&legacyResult,
               z,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionSinh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.sinh(x,
                  result: &result)
        
        vvsinh(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionCosh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.cosh(x,
                  result: &result)
        
        vvcosh(&legacyResult,
               x,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionTanh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.tanh(z,
                  result: &result)
        
        vvtanh(&legacyResult,
               z,
               &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionAsinh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.asinh(z,
                   result: &result)
        
        vvasinh(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionAcosh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.acosh(x,
                   result: &result)
        
        vvacosh(&legacyResult,
                x,
                &n)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
    }
    
    AccelerateTests.test("vForce/DoublePrecisionAtan") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vDSP.atanh(z,
                   result: &result)
        
        vvatanh(&legacyResult,
                z,
                &n)
        
        expectTrue(result.elementsEqual(legacyResult))
    }
}

runAllTests()
