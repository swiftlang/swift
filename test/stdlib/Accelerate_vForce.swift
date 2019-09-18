// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50301438
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vForceTests = TestSuite("Accelerate_vForce")

//===----------------------------------------------------------------------===//
//
//  vForce single-precision tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Float]()
    var legacyResult = [Float]()
    let x: [Float] = [2, 4, -6.876, 10.9, -100.3, 100]
    let y: [Float] = [-10, -20, 40, 60, -80, -300]
    let z: [Float] = [-0.6, 0.2, -0.1, 0.9, -0.1, 0.1]
    let count = 6
    var n: Int32 = 6
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionCeiling") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.ceil(x,
                    result: &result)
        
        vvceilf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.ceil(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionFloor") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.floor(x,
                     result: &result)
        
        vvfloorf(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.floor(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionCopySign") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.copysign(magnitudes: x,
                        signs: y,
                        result: &result)
        
        vvcopysignf(&legacyResult,
                    x,
                    y,
                    &n)
        
        let returnedResult = vForce.copysign(magnitudes: x,
                                             signs: y)
        
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionModulus") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.truncatingRemainder(dividends: x,
                                   divisors: y,
                                   result: &result)
        
        vvfmodf(&legacyResult,
                x,
                y,
                &n)
        
        let returnedResult = vForce.truncatingRemainder(dividends: x,
                                                        divisors: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionModulusParameters") {
        var scalarResult = [Float.nan]
        let scalarNumerator = [Float(20)]
        let scalarDenominator = [Float(6)]
        
        vForce.truncatingRemainder(dividends: scalarNumerator,
                                   divisors: scalarDenominator,
                                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionRemainder") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.remainder(dividends: x,
                         divisors: y,
                         result: &result)
        
        vvremainderf(&legacyResult,
                     x,
                     y,
                     &n)
        
        let returnedResult = vForce.remainder(dividends: x,
                                              divisors: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionRemainderParameters") {
        var scalarResult = [Float.nan]
        let scalarNumerator = [Float(20)]
        let scalarDenominator = [Float(6)]
        
        vForce.remainder(dividends: scalarNumerator,
                         divisors: scalarDenominator,
                         result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionIntegerTruncation") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.trunc(x,
                     result: &result)
        
        vvintf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.trunc(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionNearestInteger") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.nearestInteger(x,
                              result: &result)
        
        vvnintf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.nearestInteger(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionrsqrt") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.rsqrt(x,
                     result: &result)
        
        vvrsqrtf(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.rsqrt(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionsqrt") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.sqrt(x,
                    result: &result)
        
        vvsqrtf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.sqrt(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionReciprocal") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.reciprocal(x,
                          result: &result)
        
        vvrecf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.reciprocal(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionExp") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.exp(x,
                   result: &result)
        
        vvexpf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.exp(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionExpm1") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.expm1(x,
                     result: &result)
        
        vvexpm1f(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.expm1(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionExp2") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.exp2(x,
                    result: &result)
        
        vvexp2f(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.exp2(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionLog2") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.log2(x,
                    result: &result)
        
        vvlog2f(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.log2(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionLog10") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.log10(x,
                     result: &result)
        
        vvlog10f(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.log10(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionUnbiasedExponent") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.logb(x,
                    result: &result)
        
        vvlogbf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.logb(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionPower") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.pow(bases: x,
                   exponents: y,
                   result: &result)
        
        vvpowf(&legacyResult,
               y,
               x,
               &n)
        
        let returnedResult = vForce.pow(bases: x,
                                        exponents: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionPower") {
        var scalarResult = [Float.nan]
        let scalarBase = [Float(10)]
        let scalarExponent = [Float(2)]
        
        vForce.pow(bases: scalarBase,
                   exponents: scalarExponent,
                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 100)
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionSine") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.sin(x,
                   result: &result)
        
        vvsinf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.sin(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionSinPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.sinPi(x,
                     result: &result)
        
        vvsinpif(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.sinPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionCosine") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.cos(x,
                   result: &result)
        
        vvcosf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.cos(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionCosPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.cosPi(x,
                     result: &result)
        
        vvcospif(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.cosPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionSinCos") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        var cosResult = [Float](repeating: .nan, count: result.count)
        var legacyCosResult = [Float](repeating: .nan, count: result.count)
        
        vForce.sincos(x,
                      sinResult: &result,
                      cosResult: &cosResult)
        
        vvsincosf(&legacyResult,
                  &legacyCosResult,
                  x,
                  &n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(cosResult.elementsEqual(legacyCosResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionTangent") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.tan(x,
                   result: &result)
        
        vvtanf(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.tan(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionTanPi") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.tanPi(x,
                     result: &result)
        
        vvtanpif(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.tanPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionArcsin") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.asin(z,
                    result: &result)
        
        vvasinf(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.asin(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionArccos") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.acos(z,
                    result: &result)
        
        vvacosf(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.acos(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionArctan") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.atan(z,
                    result: &result)
        
        vvatanf(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.atan(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionSinh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.sinh(x,
                    result: &result)
        
        vvsinhf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.sinh(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionCosh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.cosh(x,
                    result: &result)
        
        vvcoshf(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.cosh(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionTanh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.tanh(z,
                    result: &result)
        
        vvtanhf(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.tanh(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionAsinh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.asinh(z,
                     result: &result)
        
        vvasinhf(&legacyResult,
                 z,
                 &n)
        
        let returnedResult = vForce.asinh(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionAcosh") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.acosh(x,
                     result: &result)
        
        vvacoshf(&legacyResult,
                 x,
                 &n)
        
        let returnedResult = vForce.acosh(x)
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/SinglePrecisionAtan") {
        result = [Float](repeating: 0, count: count)
        legacyResult = [Float](repeating: -1, count: count)
        
        vForce.atanh(z,
                     result: &result)
        
        vvatanhf(&legacyResult,
                 z,
                 &n)
        
        let returnedResult = vForce.atanh(z)
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

//===----------------------------------------------------------------------===//
//
//  vForce double precision tests
//
//===----------------------------------------------------------------------===//

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    
    var result = [Double]()
    var legacyResult = [Double]()
    let x: [Double] = [2, 4, -6.876, 10.9, -100.3, 100]
    let y: [Double] = [-10, -20, 40, 60, -80, -300]
    let z: [Double] = [-0.6, 0.2, -0.1, 0.9, -0.1, 0.1]
    let count = 6
    var n: Int32 = 6
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionCeiling") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.ceil(x,
                    result: &result)
        
        vvceil(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.ceil(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionFloor") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.floor(x,
                     result: &result)
        
        vvfloor(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.floor(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionCopySign") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.copysign(magnitudes: x,
                        signs: y,
                        result: &result)
        
        vvcopysign(&legacyResult,
                   x,
                   y,
                   &n)
        
        let returnedResult = vForce.copysign(magnitudes: x,
                                             signs: y)
        
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionModulus") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.truncatingRemainder(dividends: x,
                                   divisors: y,
                                   result: &result)
        
        vvfmod(&legacyResult,
               x,
               y,
               &n)
        
        let returnedResult = vForce.truncatingRemainder(dividends: x,
                                                        divisors: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionModulusParameters") {
        var scalarResult = [Double.nan]
        let scalarNumerator = [Double(20)]
        let scalarDenominator = [Double(6)]
        
        vForce.truncatingRemainder(dividends: scalarNumerator,
                                   divisors: scalarDenominator,
                                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionRemainder") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.remainder(dividends: x,
                         divisors: y,
                         result: &result)
        
        vvremainder(&legacyResult,
                    x,
                    y,
                    &n)
        
        let returnedResult = vForce.remainder(dividends: x,
                                              divisors: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionRemainderParameters") {
        var scalarResult = [Double.nan]
        let scalarNumerator = [Double(20)]
        let scalarDenominator = [Double(6)]
        
        vForce.remainder(dividends: scalarNumerator,
                         divisors: scalarDenominator,
                         result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 2)
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionIntegerTruncation") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.trunc(x,
                     result: &result)
        
        vvint(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.trunc(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionNearestInteger") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.nearestInteger(x,
                              result: &result)
        
        vvnint(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.nearestInteger(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionrsqrt") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.rsqrt(x,
                     result: &result)
        
        vvrsqrt(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.rsqrt(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionsqrt") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.sqrt(x,
                    result: &result)
        
        vvsqrt(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.sqrt(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionReciprocal") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.reciprocal(x,
                          result: &result)
        
        vvrec(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.reciprocal(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionExp") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.exp(x,
                   result: &result)
        
        vvexp(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.exp(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionExpm1") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.expm1(x,
                     result: &result)
        
        vvexpm1(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.expm1(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionExp2") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.exp2(x,
                    result: &result)
        
        vvexp2(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.exp2(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionLog2") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.log2(x,
                    result: &result)
        
        vvlog2(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.log2(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionLog10") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.log10(x,
                     result: &result)
        
        vvlog10(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.log10(x)
        
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionUnbiasedExponent") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.logb(x,
                    result: &result)
        
        vvlogb(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.logb(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionPower") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.pow(bases: x,
                   exponents: y,
                   result: &result)
        
        vvpow(&legacyResult,
              y,
              x,
              &n)
        
        let returnedResult = vForce.pow(bases: x,
                                        exponents: y)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionPower") {
        var scalarResult = [Double.nan]
        let scalarBase = [Double(10)]
        let scalarExponent = [Double(2)]
        
        vForce.pow(bases: scalarBase,
                   exponents: scalarExponent,
                   result: &scalarResult)
        
        expectEqual(Int(scalarResult.first!), 100)
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionSine") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.sin(x,
                   result: &result)
        
        vvsin(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.sin(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionSinPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.sinPi(x,
                     result: &result)
        
        vvsinpi(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.sinPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionCosine") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.cos(x,
                   result: &result)
        
        vvcos(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.cos(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionCosPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.cosPi(x,
                     result: &result)
        
        vvcospi(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.cosPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionSinCos") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        var cosResult = [Double](repeating: .nan, count: result.count)
        var legacyCosResult = [Double](repeating: .nan, count: result.count)
        
        vForce.sincos(x,
                      sinResult: &result,
                      cosResult: &cosResult)
        
        vvsincos(&legacyResult,
                 &legacyCosResult,
                 x,
                 &n)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(cosResult.elementsEqual(legacyCosResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionTangent") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.tan(x,
                   result: &result)
        
        vvtan(&legacyResult,
              x,
              &n)
        
        let returnedResult = vForce.tan(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionTanPi") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.tanPi(x,
                     result: &result)
        
        vvtanpi(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.tanPi(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionArcsin") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.asin(z,
                    result: &result)
        
        vvasin(&legacyResult,
               z,
               &n)
        
        let returnedResult = vForce.asin(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionArccos") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.acos(z,
                    result: &result)
        
        vvacos(&legacyResult,
               z,
               &n)
        
        let returnedResult = vForce.acos(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionArctan") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.atan(z,
                    result: &result)
        
        vvatan(&legacyResult,
               z,
               &n)
        
        let returnedResult = vForce.atan(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionSinh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.sinh(x,
                    result: &result)
        
        vvsinh(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.sinh(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionCosh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.cosh(x,
                    result: &result)
        
        vvcosh(&legacyResult,
               x,
               &n)
        
        let returnedResult = vForce.cosh(x)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionTanh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.tanh(z,
                    result: &result)
        
        vvtanh(&legacyResult,
               z,
               &n)
        
        let returnedResult = vForce.tanh(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionAsinh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.asinh(z,
                     result: &result)
        
        vvasinh(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.asinh(z)
        
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionAcosh") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.acosh(x,
                     result: &result)
        
        vvacosh(&legacyResult,
                x,
                &n)
        
        let returnedResult = vForce.acosh(x)
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(legacyResult.filter{!$0.isNaN}))
        expectTrue(result.filter{!$0.isNaN}.elementsEqual(returnedResult.filter{!$0.isNaN}))
    }
    
    Accelerate_vForceTests.test("vForce/DoublePrecisionAtan") {
        result = [Double](repeating: 0, count: count)
        legacyResult = [Double](repeating: -1, count: count)
        
        vForce.atanh(z,
                     result: &result)
        
        vvatanh(&legacyResult,
                z,
                &n)
        
        let returnedResult = vForce.atanh(z)
        expectTrue(result.elementsEqual(legacyResult))
        expectTrue(result.elementsEqual(returnedResult))
    }
}

runAllTests()
