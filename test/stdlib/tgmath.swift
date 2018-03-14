// RUN: %target-run-simple-swift
// REQUIRES: executable_test

#if os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
  import Glibc
  // FIXME: this is a quick hack for non Darwin platforms
  // where they doesn't have CoreGraphics module.
  #if arch(i386) || arch(arm)
    typealias CGFloat = Float
  #elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
    typealias CGFloat = Double
  #endif
#else
import Darwin.C.tgmath
import CoreGraphics
#endif
import StdlibUnittest

let MathTests = TestSuite("TGMath")

func expectEqualWithAccuracy(_ actualDouble: Double, _ expectedDouble: Double, ulps: Int, _ message: String = "", file: String = #file, line: UInt = #line) {
    let epsilonDouble = Double(ulps) * expectedDouble.ulp
    let minDouble = expectedDouble - epsilonDouble
    let maxDouble = expectedDouble + epsilonDouble
    let messageDouble = "Expected: \(expectedDouble) != Actual: \(actualDouble)"
    expectTrue((actualDouble.isNaN && expectedDouble.isNaN) || (actualDouble >= minDouble && actualDouble <= maxDouble), messageDouble, file: file, line: line)
}

func expectEqualWithAccuracy(_ actualFloat: Float, _ expectedFloat: Float, ulps: Int, _ message: String = "", file: String = #file, line: UInt = #line) {
    let epsilonFloat = Float(ulps) * expectedFloat.ulp
    let minFloat = expectedFloat - epsilonFloat
    let maxFloat = expectedFloat + epsilonFloat
    let messageFloat = "Expected: \(expectedFloat) != Actual: \(actualFloat) (actualFloat.isNaN=\(actualFloat.isNaN) , expectedFloat.isNaN=\(expectedFloat.isNaN), minFloat=\(minFloat), maxFloat = \(maxFloat)"
    expectTrue((actualFloat.isNaN && expectedFloat.isNaN) || (actualFloat >= minFloat && actualFloat <= maxFloat), messageFloat, file: file, line: line)
}

func expectEqualWithAccuracy(_ actualDouble: Double, _ actualFloat: Float, _ actualCGFloat: CGFloat, _ expected: Double, ulps: Int, _ message: String = "", file: String = #file, line: UInt = #line) {
    expectEqualWithAccuracy(actualDouble, expected, ulps: ulps, message, file: file, line: line)
    expectEqualWithAccuracy(actualFloat, Float(expected), ulps: ulps, message, file: file, line: line)

#if arch(i386) || arch(arm)
    expectEqualWithAccuracy(Float(actualCGFloat), Float(expected), ulps: ulps, message, file: file, line: line)
#else
    expectEqualWithAccuracy(Double(actualCGFloat), Double(expected), ulps: ulps, message, file: file, line: line)
#endif
}

// inputs
let dx = Double(0.1)
let dy = Double(2.2)
let dz = Double(3.3)

let fx = Float(0.1)
let fy = Float(2.2)
let fz = Float(3.3)

let gx = CGFloat(0.1)
let gy = CGFloat(2.2)
let gz = CGFloat(3.3)

let ix = Int(11)

MathTests.test("Unary functions") {
  expectEqualWithAccuracy(acos(dx), acos(fx), acos(gx), 1.4706289056333368, ulps: 1)

  expectEqualWithAccuracy(asin(dx), asin(fx), asin(gx), 0.1001674211615598, ulps: 1)

  expectEqualWithAccuracy(atan(dx), atan(fx), atan(gx), 0.09966865249116204, ulps: 1)

  expectEqualWithAccuracy(cos(dx), cos(fx), cos(gx), 0.9950041652780257, ulps: 1)

  expectEqualWithAccuracy(sin(dx), sin(fx), sin(gx), 0.09983341664682815, ulps: 1)

  expectEqualWithAccuracy(tan(dx), tan(fx), tan(gx), 0.10033467208545055, ulps: 1)

  expectEqualWithAccuracy(acosh(dx), acosh(fx), acosh(gx), Double.nan, ulps: 1)

  expectEqualWithAccuracy(asinh(dx), asinh(fx), asinh(gx), 0.09983407889920758, ulps: 1)

  expectEqualWithAccuracy(atanh(dx), atanh(fx), atanh(gx), 0.10033534773107558, ulps: 1)

  expectEqualWithAccuracy(cosh(dx), cosh(fx), cosh(gx), 1.0050041680558035, ulps: 1)

  expectEqualWithAccuracy(sinh(dx), sinh(fx), sinh(gx), 0.10016675001984403, ulps: 1)

  expectEqualWithAccuracy(tanh(dx), tanh(fx), tanh(gx), 0.09966799462495582, ulps: 1)

  expectEqualWithAccuracy(exp(dx), exp(fx), exp(gx), 1.1051709180756477, ulps: 1)

  expectEqualWithAccuracy(exp2(dx), exp2(fx), exp2(gx), 1.0717734625362931, ulps: 1)

  expectEqualWithAccuracy(expm1(dx), expm1(fx), expm1(gx), 0.10517091807564763, ulps: 1)

  expectEqualWithAccuracy(log(dx), log(fx), log(gx), -2.3025850929940455, ulps: 1)

  expectEqualWithAccuracy(log10(dx), log10(fx), log10(gx), -1.0, ulps: 1)

  expectEqualWithAccuracy(log2(dx), log2(fx), log2(gx), -3.321928094887362, ulps: 1)

  expectEqualWithAccuracy(log1p(dx), log1p(fx), log1p(gx), 0.09531017980432487, ulps: 1)

  expectEqualWithAccuracy(logb(dx), logb(fx), logb(gx), -4.0, ulps: 1)

  expectEqualWithAccuracy(fabs(dx), fabs(fx), fabs(gx), 0.1, ulps: 1)

  expectEqualWithAccuracy(cbrt(dx), cbrt(fx), cbrt(gx), 0.4641588833612779, ulps: 1)

  expectEqualWithAccuracy(sqrt(dx), sqrt(fx), sqrt(gx), 0.31622776601683794, ulps: 1)

  expectEqualWithAccuracy(erf(dx), erf(fx), erf(gx), 0.1124629160182849, ulps: 1)

  expectEqualWithAccuracy(erfc(dx), erfc(fx), erfc(gx), 0.8875370839817152, ulps: 1)

  expectEqualWithAccuracy(tgamma(dx), tgamma(fx), tgamma(gx), 9.51350769866873, ulps: 1)

  expectEqualWithAccuracy(ceil(dx), ceil(fx), ceil(gx), 1.0, ulps: 0)

  expectEqualWithAccuracy(floor(dx), floor(fx), floor(gx), 0.0, ulps: 0)

  expectEqualWithAccuracy(nearbyint(dx), nearbyint(fx), nearbyint(gx), 0.0, ulps: 0)

  expectEqualWithAccuracy(rint(dx), rint(fx), rint(gx), 0.0, ulps: 0)

  expectEqualWithAccuracy(round(dx), round(fx), round(gx), 0.0, ulps: 0)

  expectEqualWithAccuracy(trunc(dx), trunc(fx), trunc(gx), 0.0, ulps: 0)
}

MathTests.test("Binary functions") {
    expectEqualWithAccuracy(atan2(dx, dy), atan2(fx, fy), atan2(gx, gy), 0.04542327942157701, ulps: 1)
    expectEqualWithAccuracy(hypot(dx, dy), hypot(fx, fy), hypot(gx, gy), 2.202271554554524, ulps: 1)
    expectEqualWithAccuracy(pow(dx, dy), pow(fx, fy), pow(gx, gy), 0.00630957344480193, ulps: 1)
    expectEqualWithAccuracy(fmod(dx, dy), fmod(fx, fy), fmod(gx, gy), 0.1, ulps: 0)
    expectEqualWithAccuracy(remainder(dx, dy), remainder(fx, fy), remainder(gx, gy), 0.1, ulps: 0)
    expectEqualWithAccuracy(copysign(dx, dy), copysign(fx, fy), copysign(gx, gy), 0.1, ulps: 0)

    expectEqual(nextafter(dx, dy), dx.nextUp)
    expectEqual(nextafter(fx, fy), fx.nextUp)
    expectEqual(nextafter(gx, gy), gx.nextUp)
    expectEqual(nextafter(dx, dx - 1.0), dx.nextDown)
    expectEqual(nextafter(fx, fx - Float(1.0)), fx.nextDown)
    expectEqual(nextafter(gx, gx - CGFloat(1.0)), gx.nextDown)

    expectEqualWithAccuracy(fdim(dx, dy), fdim(fx, fy), fdim(gx, gy), 0.0, ulps: 0)
    expectEqualWithAccuracy(fmax(dx, dy), fmax(fx, fy), fmax(gx, gy), 2.2, ulps: 0)
    expectEqualWithAccuracy(fmin(dx, dy), fmin(fx, fy), fmin(gx, gy), 0.1, ulps: 0)
}

MathTests.test("Other functions") {

  expectEqual(modf(dy), (2.0, 0.20000000000000018))
  expectEqual(modf(fy), (2.0, 0.20000005))
#if arch(i386) || arch(arm)
  expectEqual(modf(gy), (2.0, 0.20000005))
#else
  expectEqual(modf(gy), (2.0, 0.20000000000000018))
#endif

  expectEqualWithAccuracy(ldexp(dx, ix), ldexp(fx, ix), ldexp(gx, ix), 204.8, ulps: 0)

  expectEqual(frexp(dy), (0.55, 2))
  expectEqual(frexp(fy), (0.55, 2))
  expectEqual(frexp(gy), (0.55, 2))

  expectEqual(ilogb(dy), 1)
  expectEqual(ilogb(fy), 1)
  expectEqual(ilogb(gy), 1)

  expectEqualWithAccuracy(scalbn(dx, ix), scalbn(fx, ix), scalbn(gx, ix), 204.8, ulps: 0)

  expectEqual(lgamma(dx).1, 1)
  expectEqual(lgamma(fx).1, 1)
  expectEqual(lgamma(gx).1, 1)
  expectEqualWithAccuracy(lgamma(dx).0, lgamma(fx).0, lgamma(gx).0, 2.2527126517342055, ulps: 1)

  expectEqual(remquo(dz, dy).1, 1)
  expectEqual(remquo(fz, fy).1, 1)
  expectEqual(remquo(gz, gy).1, 1)
  expectEqualWithAccuracy(remquo(dz, dy).0, remquo(fz, fy).0, remquo(gz, gy).0, 1.0999999999999996, ulps: 1)

  expectEqualWithAccuracy(nan("12345"), nan("12345"), nan("12345"), Double.nan, ulps: 0)

  expectEqualWithAccuracy(fma(dx, dy, dz), fma(fx, fy, fz), fma(gx, gy, gz), 3.52, ulps: 0)

  expectEqualWithAccuracy(j0(dx), 0.99750156206604, ulps: 1)
  expectEqualWithAccuracy(j1(dx), 0.049937526036242, ulps: 1)
  expectEqualWithAccuracy(jn(ix, dx), 1.2229926610356451e-22, ulps: 1)
  expectEqualWithAccuracy(y0(dx), -1.5342386513503667, ulps: 1)
  expectEqualWithAccuracy(y1(dx), -6.458951094702027, ulps: 1)
  expectEqualWithAccuracy(yn(ix, dx), -2.3662012944869576e+20, ulps: 1)
}

runAllTests()
