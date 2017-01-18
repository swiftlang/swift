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

func print3(_ op: String, _ d: Double, _ f: Float, _ g: CGFloat) -> String {
#if arch(i386) || arch(arm)
  if (f != Float(g)  &&  f.isNaN != g.isNaN) {
    return "\(op)(CGFloat) got \(g) instead of \(f)"
  }
#else
  if (d != Double(g)  &&  d.isNaN != g.isNaN) {
    return "\(op)(CGFloat) got \(g) instead of \(d)"
  }
#endif
  return "\(op) \(d) \(f) \(op)"
}

func print3(_ op: String, _ d: Bool, _ f: Bool, _ g: Bool) -> String {
#if arch(i386) || arch(arm)
  if (f != g) {
    return "\(op)(CGFloat) got \(g) instead of \(f)"
  }
#else
  if (d != g) {
    return "\(op)(CGFloat) got \(g) instead of \(d)"
  }
#endif
  return "\(op) \(d) \(f) \(op)"
}

func print3(_ op: String, _ d: Int, _ f: Int, _ g: Int) -> String {
#if arch(i386) || arch(arm)
  if (f != g) {
    return "\(op)(CGFloat) got \(g) instead of \(f)"
  }
#else
  if (d != g) {
    return "\(op)(CGFloat) got \(g) instead of \(d)"
  }
#endif
  return "\(op) \(d) \(f) \(op)"
}


func print6(_ op: String, _ d1: Double, _ d2: Double,
  _ f1: Float, _ f2: Float, _ g1: CGFloat, _ g2: CGFloat)  -> String
{
#if arch(i386) || arch(arm)
  if (f1 != Float(g1)  ||  f2 != Float(g2)) {
    return "\(op)(CGFloat) got \(g1),\(g2) instead of \(f1),\(f2)"
  }
#else
  if (d1 != Double(g1)  ||  d2 != Double(g2)) {
    return "\(op)(CGFloat) got \(g1),\(g2) instead of \(d1),\(d2)"
  }
#endif
  return "\(op) \(d1),\(d2) \(f1),\(f2) \(op)"
}

func print6(_ op: String, _ d1: Double, _ di: Int,
  _ f1: Float, _ fi: Int, _ g1: CGFloat, _ gi: Int) -> String
{
#if arch(i386) || arch(arm)
  if (f1 != Float(g1) || fi != gi) {
    return "\(op)(CGFloat) got \(g1),\(gi) instead of \(f1),\(fi)"
  }
#else
  if (d1 != Double(g1)  ||  di != gi) {
    return "\(op)(CGFloat) got \(g1),\(gi) instead of \(d1),\(di)"
  }
#endif
  return "\(op) \(d1),\(di) \(f1),\(fi) \(op)"
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

// outputs
var d1: Double = 0, d2: Double = 0
var f1: Float = 0, f2: Float = 0
var g1: CGFloat = 0, g2: CGFloat = 0
var i1 = 0, i2 = 0, i3 = 0
var b1 = false, b2 = false, b3 = false

MathTests.test("Unary functions") {
  (d1, f1, g1) = (acos(dx), acos(fx), acos(gx))
  expectEqual("acos 1.47062890563334 1.47063 acos", print3("acos", d1, f1, g1))

  (d1, f1, g1) = (asin(dx), asin(fx), asin(gx))
  expectEqual("asin 0.10016742116156 0.100167 asin", print3("asin", d1, f1, g1))

  (d1, f1, g1) = (atan(dx), atan(fx), atan(gx))
  expectEqual("atan 0.099668652491162 0.0996687 atan",
    print3("atan", d1, f1, g1))

  (d1, f1, g1) = (cos(dx), cos(fx), cos(gx))
  expectEqual("cos 0.995004165278026 0.995004 cos", print3("cos", d1, f1, g1))

  (d1, f1, g1) = (sin(dx), sin(fx), sin(gx))
  expectEqual("sin 0.0998334166468282 0.0998334 sin", print3("sin", d1, f1, g1))

  (d1, f1, g1) = (tan(dx), tan(fx), tan(gx))
  expectEqual("tan 0.100334672085451 0.100335 tan", print3("tan", d1, f1, g1))

  (d1, f1, g1) = (acosh(dx), acosh(fx), acosh(gx))
  expectEqual("acosh nan nan acosh", print3("acosh", d1, f1, g1))

  (d1, f1, g1) = (asinh(dx), asinh(fx), asinh(gx))
  expectEqual("asinh 0.0998340788992076 0.0998341 asinh",
    print3("asinh", d1, f1, g1))

  (d1, f1, g1) = (atanh(dx), atanh(fx), atanh(gx))
  expectEqual("atanh 0.100335347731076 0.100335 atanh",
    print3("atanh", d1, f1, g1))

  (d1, f1, g1) = (cosh(dx), cosh(fx), cosh(gx))
  expectEqual("cosh 1.0050041680558 1.005 cosh", print3("cosh", d1, f1, g1))

  (d1, f1, g1) = (sinh(dx), sinh(fx), sinh(gx))
  expectEqual("sinh 0.100166750019844 0.100167 sinh", print3("sinh", d1, f1, g1))

  (d1, f1, g1) = (tanh(dx), tanh(fx), tanh(gx))
  expectEqual("tanh 0.0996679946249558 0.099668 tanh",
    print3("tanh", d1, f1, g1))

  (d1, f1, g1) = (exp(dx), exp(fx), exp(gx))
  expectEqual("exp 1.10517091807565 1.10517 exp", print3("exp", d1, f1, g1))

  (d1, f1, g1) = (exp2(dx), exp2(fx), exp2(gx))
  expectEqual("exp2 1.07177346253629 1.07177 exp2", print3("exp2", d1, f1, g1))

  (d1, f1, g1) = (expm1(dx), expm1(fx), expm1(gx))
  expectEqual("expm1 0.105170918075648 0.105171 expm1",
    print3("expm1", d1, f1, g1))

  (d1, f1, g1) = (log(dx), log(fx), log(gx))
  expectEqual("log -2.30258509299405 -2.30259 log", print3("log", d1, f1, g1))

  (d1, f1, g1) = (log10(dx), log10(fx), log10(gx))
  expectEqual("log10 -1.0 -1.0 log10", print3("log10", d1, f1, g1))

  (d1, f1, g1) = (log2(dx), log2(fx), log2(gx))
  expectEqual("log2 -3.32192809488736 -3.32193 log2", print3("log2", d1, f1, g1))

  (d1, f1, g1) = (log1p(dx), log1p(fx), log1p(gx))
  expectEqual("log1p 0.0953101798043249 0.0953102 log1p",
    print3("log1p", d1, f1, g1))

  (d1, f1, g1) = (logb(dx), logb(fx), logb(gx))
  expectEqual("logb -4.0 -4.0 logb", print3("logb", d1, f1, g1))

  (d1, f1, g1) = (fabs(dx), fabs(fx), fabs(gx))
  expectEqual("fabs 0.1 0.1 fabs", print3("fabs", d1, f1, g1))

  (d1, f1, g1) = (cbrt(dx), cbrt(fx), cbrt(gx))
  expectEqual("cbrt 0.464158883361278 0.464159 cbrt", print3("cbrt", d1, f1, g1))

  (d1, f1, g1) = (sqrt(dx), sqrt(fx), sqrt(gx))
  expectEqual("sqrt 0.316227766016838 0.316228 sqrt", print3("sqrt", d1, f1, g1))

  (d1, f1, g1) = (erf(dx), erf(fx), erf(gx))
  expectEqual("erf 0.112462916018285 0.112463 erf", print3("erf", d1, f1, g1))

  (d1, f1, g1) = (erfc(dx), erfc(fx), erfc(gx))
  expectEqual("erfc 0.887537083981715 0.887537 erfc", print3("erfc", d1, f1, g1))

  (d1, f1, g1) = (tgamma(dx), tgamma(fx), tgamma(gx))
  expectEqual("tgamma 9.51350769866873 9.51351 tgamma",
    print3("tgamma", d1, f1, g1))

  (d1, f1, g1) = (ceil(dx), ceil(fx), ceil(gx))
  expectEqual("ceil 1.0 1.0 ceil", print3("ceil", d1, f1, g1))

  (d1, f1, g1) = (floor(dx), floor(fx), floor(gx))
  expectEqual("floor 0.0 0.0 floor", print3("floor", d1, f1, g1))

  (d1, f1, g1) = (nearbyint(dx), nearbyint(fx), nearbyint(gx))
  expectEqual("nearbyint 0.0 0.0 nearbyint", print3("nearbyint", d1, f1, g1))

  (d1, f1, g1) = (rint(dx), rint(fx), rint(gx))
  expectEqual("rint 0.0 0.0 rint", print3("rint", d1, f1, g1))

  (d1, f1, g1) = (round(dx), round(fx), round(gx))
  expectEqual("round 0.0 0.0 round", print3("round", d1, f1, g1))

  (d1, f1, g1) = (trunc(dx), trunc(fx), trunc(gx))
  expectEqual("trunc 0.0 0.0 trunc", print3("trunc", d1, f1, g1))
}

MathTests.test("Binary functions") {

  (d1, f1, g1) = (atan2(dx, dy), atan2(fx, fy), atan2(gx, gy))
  expectEqual("atan2 0.045423279421577 0.0454233 atan2",
    print3("atan2", d1, f1, g1))

  (d1, f1, g1) = (hypot(dx, dy), hypot(fx, fy), hypot(gx, gy))
  expectEqual("hypot 2.20227155455452 2.20227 hypot",
    print3("hypot", d1, f1, g1))

  (d1, f1, g1) = (pow(dx, dy), pow(fx, fy), pow(gx, gy))
  expectEqual("pow 0.00630957344480193 0.00630957 pow",
    print3("pow", d1, f1, g1))

  (d1, f1, g1) = (fmod(dx, dy), fmod(fx, fy), fmod(gx, gy))
  expectEqual("fmod 0.1 0.1 fmod", print3("fmod", d1, f1, g1))

  (d1, f1, g1) = (remainder(dx, dy), remainder(fx, fy), remainder(gx, gy))
  expectEqual("remainder 0.1 0.1 remainder", print3("remainder", d1, f1, g1))

  (d1, f1, g1) = (copysign(dx, dy), copysign(fx, fy), copysign(gx, gy))
  expectEqual("copysign 0.1 0.1 copysign", print3("copysign", d1, f1, g1))

  (d1, f1, g1) = (nextafter(dx, dy), nextafter(fx, fy), nextafter(gx, gy))
  expectEqual("nextafter 0.1 0.1 nextafter", print3("nextafter", d1, f1, g1))

  (d1, f1, g1) = (fdim(dx, dy), fdim(fx, fy), fdim(gx, gy))
  expectEqual("fdim 0.0 0.0 fdim", print3("fdim", d1, f1, g1))

  (d1, f1, g1) = (fmax(dx, dy), fmax(fx, fy), fmax(gx, gy))
  expectEqual("fmax 2.2 2.2 fmax", print3("fmax", d1, f1, g1))

  (d1, f1, g1) = (fmin(dx, dy), fmin(fx, fy), fmin(gx, gy))
  expectEqual("fmin 0.1 0.1 fmin", print3("fmin", d1, f1, g1))
}

MathTests.test("Other functions") {

  (d1, d2) = modf(dy)
  (f1, f2) = modf(fy)
  (g1, g2) = modf(gy)
  expectEqual("modf 2.0,0.2 2.0,0.2 modf", print6("modf", d1,d2, f1,f2, g1,g2))

  (d1, f1, g1) = (ldexp(dx, ix), ldexp(fx, ix), ldexp(gx, ix))
  expectEqual("ldexp 204.8 204.8 ldexp", print3("ldexp", d1, f1, g1))

  (d1, i1) = frexp(dy)
  (f1, i2) = frexp(fy)
  (g1, i3) = frexp(gy)
  expectEqual("frexp 0.55,2 0.55,2 frexp", print6("frexp", d1,i1, f1,i2, g1,i3))

  (i1, i2, i3) = (ilogb(dy), ilogb(fy), ilogb(gy))
  expectEqual("ilogb 1 1 ilogb", print3("ilogb", i1, i2, i3))

  (d1, f1, g1) = (scalbn(dx, ix), scalbn(fx, ix), scalbn(gx, ix))
  expectEqual("scalbn 204.8 204.8 scalbn", print3("scalbn", d1, f1, g1))

  (d1, i1) = lgamma(dx)
  (f1, i2) = lgamma(fx)
  (g1, i3) = lgamma(gx)
  expectEqual("lgamma 2.25271265173421,1 2.25271,1 lgamma", print6("lgamma",
 d1,i1, f1,i2, g1,i3))

  (d1, i1) = remquo(dz, dy)
  (f1, i2) = remquo(fz, fy)
  (g1, i3) = remquo(gz, gy)
  expectEqual("remquo 1.1,1 1.1,1 remquo", print6("remquo", d1,i1, f1,i2, g1,i3))

  (d1, f1, g1) = (nan("12345"), nan("12345"), nan("12345"))
  expectEqual("nan nan nan nan", print3("nan", d1, f1, g1))

  (d1, f1, g1) = (fma(dx, dy, dz), fma(fx, fy, fz), fma(gx, gy, gz))
  expectEqual("fma 3.52 3.52 fma", print3("fma", d1, f1, g1))

  expectEqual("j0 0.99750156206604 j0", "j0 \(j0(dx)) j0")
  expectEqual("j1 0.049937526036242 j1", "j1 \(j1(dx)) j1")
  expectEqual("jn 1.22299266103565e-22 jn", "jn \(jn(ix, dx)) jn")
  expectEqual("y0 -1.53423865135037 y0", "y0 \(y0(dx)) y0")
  expectEqual("y1 -6.45895109470203 y1", "y1 \(y1(dx)) y1")

  d1 = yn(ix, dx)
  expectEqual("yn -2.36620129448696e+20 yn", "yn \(d1) yn")
}

runAllTests()
