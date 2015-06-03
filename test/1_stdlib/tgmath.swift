// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// XFAIL: linux

import Darwin.C.tgmath
import CoreGraphics

// verifiers

func print3(op: String, _ d: Double, _ f: Float, _ g: CGFloat) {
#if arch(i386) || arch(arm)
  if (f != Float(g)  &&  f.isNaN != g.isNaN) {
    print("\(op)(CGFloat) got \(g) instead of \(f)")
  }
#else
  if (d != Double(g)  &&  d.isNaN != g.isNaN) {
    print("\(op)(CGFloat) got \(g) instead of \(d)")
  }
#endif
  print("\(op) \(d) \(f) \(op)")
}

func print3(op: String, _ d: Bool, _ f: Bool, _ g: Bool) {
#if arch(i386) || arch(arm)
  if (f != g) {
    print("\(op)(CGFloat) got \(g) instead of \(f)")
  }
#else
  if (d != g) {
    print("\(op)(CGFloat) got \(g) instead of \(d)")
  }
#endif
  print("\(op) \(d) \(f) \(op)")
}

func print3(op: String, _ d: Int, _ f: Int, _ g: Int) {
#if arch(i386) || arch(arm)
  if (f != g) {
    print("\(op)(CGFloat) got \(g) instead of \(f)")
  }
#else
  if (d != g) {
    print("\(op)(CGFloat) got \(g) instead of \(d)")
  }
#endif
  print("\(op) \(d) \(f) \(op)")
}

func print6(op: String, _ d1: Double, _ d2: Double, 
  _ f1: Float, _ f2: Float, _ g1: CGFloat, _ g2: CGFloat) 
{
#if arch(i386) || arch(arm)
  if (f1 != Float(g1)  ||  f2 != Float(g2)) {
    print("\(op)(CGFloat) got \(g1),\(g2) instead of \(f1),\(f2)")
  }
#else
  if (d1 != Double(g1)  ||  d2 != Double(g2)) {
    print("\(op)(CGFloat) got \(g1),\(g2) instead of \(d1),\(d2)")
  }
#endif
  print("\(op) \(d1),\(d2) \(f1),\(f2) \(op)")
}

func print6(op: String, _ d1: Double, _ di: Int, 
  _ f1: Float, _ fi: Int, _ g1: CGFloat, _ gi: Int)
{
#if arch(i386) || arch(arm)
  if (f1 != Float(g1)  ||  fi != gi) {
    print("\(op)(CGFloat) got \(g1),\(gi) instead of \(f1),\(fi)")
  }
#else
  if (d1 != Double(g1)  ||  di != gi) {
    print("\(op)(CGFloat) got \(g1),\(gi) instead of \(d1),\(di)")
  }
#endif
  print("\(op) \(d1),\(di) \(f1),\(fi) \(op)")
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
var d1, d2: Double
var f1, f2: Float
var g1, g2: CGFloat
var i1, i2, i3: Int
var b1, b2, b3: Bool

// The order of these tests matches tgmath.swift.gyb.

print("Shopping is hard, let's do math")
// CHECK: Shopping is hard, let's do math

// Unary functions

d1 = acos(dx)
f1 = acos(fx)
g1 = acos(gx)
print3("acos", d1, f1, g1)
// CHECK-NEXT: acos 1.47062890563334 1.47063 acos

d1 = asin(dx)
f1 = asin(fx)
g1 = asin(gx)
print3("asin", d1, f1, g1)
// CHECK-NEXT: asin 0.10016742116156 0.100167 asin

d1 = atan(dx)
f1 = atan(fx)
g1 = atan(gx)
print3("atan", d1, f1, g1)
// CHECK-NEXT: atan 0.099668652491162 0.0996687 atan

d1 = cos(dx)
f1 = cos(fx)
g1 = cos(gx)
print3("cos", d1, f1, g1)
// CHECK-NEXT: cos 0.995004165278026 0.995004 cos

d1 = sin(dx)
f1 = sin(fx)
g1 = sin(gx)
print3("sin", d1, f1, g1)
// CHECK-NEXT: sin 0.0998334166468282 0.0998334 sin

d1 = tan(dx)
f1 = tan(fx)
g1 = tan(gx)
print3("tan", d1, f1, g1)
// CHECK-NEXT: tan 0.100334672085451 0.100335 tan


d1 = acosh(dx)
f1 = acosh(fx)
g1 = acosh(gx)
print3("acosh", d1, f1, g1)
// CHECK-NEXT: acosh nan nan acosh

d1 = asinh(dx)
f1 = asinh(fx)
g1 = asinh(gx)
print3("asinh", d1, f1, g1)
// CHECK-NEXT: asinh 0.0998340788992076 0.0998341 asinh

d1 = atanh(dx)
f1 = atanh(fx)
g1 = atanh(gx)
print3("atanh", d1, f1, g1)
// CHECK-NEXT: atanh 0.100335347731076 0.100335 atanh

d1 = cosh(dx)
f1 = cosh(fx)
g1 = cosh(gx)
print3("cosh", d1, f1, g1)
// CHECK-NEXT: cosh 1.0050041680558 1.005 cosh

d1 = sinh(dx)
f1 = sinh(fx)
g1 = sinh(gx)
print3("sinh", d1, f1, g1)
// CHECK-NEXT: sinh 0.100166750019844 0.100167 sinh

d1 = tanh(dx)
f1 = tanh(fx)
g1 = tanh(gx)
print3("tanh", d1, f1, g1)
// CHECK-NEXT: tanh 0.0996679946249558 0.099668 tanh


d1 = exp(dx)
f1 = exp(fx)
g1 = exp(gx)
print3("exp", d1, f1, g1)
// CHECK-NEXT: exp 1.10517091807565 1.10517 exp

d1 = exp2(dx)
f1 = exp2(fx)
g1 = exp2(gx)
print3("exp2", d1, f1, g1)
// CHECK-NEXT: exp2 1.07177346253629 1.07177 exp2

d1 = expm1(dx)
f1 = expm1(fx)
g1 = expm1(gx)
print3("expm1", d1, f1, g1)
// CHECK-NEXT: expm1 0.105170918075648 0.105171 expm1


d1 = log(dx)
f1 = log(fx)
g1 = log(gx)
print3("log", d1, f1, g1)
// CHECK-NEXT: log -2.30258509299405 -2.30259 log

d1 = log10(dx)
f1 = log10(fx)
g1 = log10(gx)
print3("log10", d1, f1, g1)
// CHECK-NEXT: log10 -1.0 -1.0 log10

d1 = log2(dx)
f1 = log2(fx)
g1 = log2(gx)
print3("log2", d1, f1, g1)
// CHECK-NEXT: log2 -3.32192809488736 -3.32193 log2

d1 = log1p(dx)
f1 = log1p(fx)
g1 = log1p(gx)
print3("log1p", d1, f1, g1)
// CHECK-NEXT: log1p 0.0953101798043249 0.0953102 log1p

d1 = logb(dx)
f1 = logb(fx)
g1 = logb(gx)
print3("logb", d1, f1, g1)
// CHECK-NEXT: logb -4.0 -4.0 logb


d1 = fabs(dx)
f1 = fabs(fx)
g1 = fabs(gx)
print3("fabs", d1, f1, g1)
// CHECK-NEXT: fabs 0.1 0.1 fabs

d1 = cbrt(dx)
f1 = cbrt(fx)
g1 = cbrt(gx)
print3("cbrt", d1, f1, g1)
// CHECK-NEXT: cbrt 0.464158883361278 0.464159 cbrt

d1 = sqrt(dx)
f1 = sqrt(fx)
g1 = sqrt(gx)
print3("sqrt", d1, f1, g1)
// CHECK-NEXT: sqrt 0.316227766016838 0.316228 sqrt

d1 = erf(dx)
f1 = erf(fx)
g1 = erf(gx)
print3("erf", d1, f1, g1)
// CHECK-NEXT: erf 0.112462916018285 0.112463 erf

d1 = erfc(dx)
f1 = erfc(fx)
g1 = erfc(gx)
print3("erfc", d1, f1, g1)
// CHECK-NEXT: erfc 0.887537083981715 0.887537 erfc

d1 = tgamma(dx)
f1 = tgamma(fx)
g1 = tgamma(gx)
print3("tgamma", d1, f1, g1)
// CHECK-NEXT: tgamma 9.51350769866873 9.51351 tgamma


d1 = ceil(dx)
f1 = ceil(fx)
g1 = ceil(gx)
print3("ceil", d1, f1, g1)
// CHECK-NEXT: ceil 1.0 1.0 ceil

d1 = floor(dx)
f1 = floor(fx)
g1 = floor(gx)
print3("floor", d1, f1, g1)
// CHECK-NEXT: floor 0.0 0.0 floor

d1 = nearbyint(dx)
f1 = nearbyint(fx)
g1 = nearbyint(gx)
print3("nearbyint", d1, f1, g1)
// CHECK-NEXT: nearbyint 0.0 0.0 nearbyint

d1 = rint(dx)
f1 = rint(fx)
g1 = rint(gx)
print3("rint", d1, f1, g1)
// CHECK-NEXT: rint 0.0 0.0 rint

d1 = round(dx)
f1 = round(fx)
g1 = round(gx)
print3("round", d1, f1, g1)
// CHECK-NEXT: round 0.0 0.0 round

d1 = trunc(dx)
f1 = trunc(fx)
g1 = trunc(gx)
print3("trunc", d1, f1, g1)
// CHECK-NEXT: trunc 0.0 0.0 trunc


// Binary functions

d1 = atan2(dx, dy)
f1 = atan2(fx, fy)
g1 = atan2(gx, gy)
print3("atan2", d1, f1, g1)
// CHECK-NEXT: atan2 0.045423279421577 0.0454233 atan2

d1 = hypot(dx, dy)
f1 = hypot(fx, fy)
g1 = hypot(gx, gy)
print3("hypot", d1, f1, g1)
// CHECK-NEXT: hypot 2.20227155455452 2.20227 hypot

d1 = pow(dx, dy)
f1 = pow(fx, fy)
g1 = pow(gx, gy)
print3("pow", d1, f1, g1)
// CHECK-NEXT: pow 0.00630957344480193 0.00630957 pow

d1 = fmod(dx, dy)
f1 = fmod(fx, fy)
g1 = fmod(gx, gy)
print3("fmod", d1, f1, g1)
// CHECK-NEXT: fmod 0.1 0.1 fmod

d1 = remainder(dx, dy)
f1 = remainder(fx, fy)
g1 = remainder(gx, gy)
print3("remainder", d1, f1, g1)
// CHECK-NEXT: remainder 0.1 0.1 remainder

d1 = copysign(dx, dy)
f1 = copysign(fx, fy)
g1 = copysign(gx, gy)
print3("copysign", d1, f1, g1)
// CHECK-NEXT: copysign 0.1 0.1 copysign

d1 = nextafter(dx, dy)
f1 = nextafter(fx, fy)
g1 = nextafter(gx, gy)
print3("nextafter", d1, f1, g1)
// CHECK-NEXT: nextafter 0.1 0.1 nextafter

d1 = fdim(dx, dy)
f1 = fdim(fx, fy)
g1 = fdim(gx, gy)
print3("fdim", d1, f1, g1)
// CHECK-NEXT: fdim 0.0 0.0 fdim

d1 = fmax(dx, dy)
f1 = fmax(fx, fy)
g1 = fmax(gx, gy)
print3("fmax", d1, f1, g1)
// CHECK-NEXT: fmax 2.2 2.2 fmax

d1 = fmin(dx, dy)
f1 = fmin(fx, fy)
g1 = fmin(gx, gy)
print3("fmin", d1, f1, g1)
// CHECK-NEXT: fmin 0.1 0.1 fmin


// Other functions

i1 = fpclassify(dx)
i2 = fpclassify(fx)
i3 = fpclassify(gx)
print3("fpclassify", i1, i2, i3)
// CHECK-NEXT: fpclassify 4 4 fpclassify

b1 = isnormal(dx)
b2 = isnormal(fx)
b3 = isnormal(gx)
print3("isnormal", b1, b2, b3)
// CHECK-NEXT: isnormal true true isnormal

b1 = isfinite(dx)
b2 = isfinite(fx)
b3 = isfinite(gx)
print3("isfinite", b1, b2, b3)
// CHECK-NEXT: isfinite true true isfinite

b1 = isinf(dx)
b2 = isinf(fx)
b3 = isinf(gx)
print3("isinf", b1, b2, b3)
// CHECK-NEXT: isinf false false isinf

b1 = isnan(dx)
b2 = isnan(fx)
b3 = isnan(gx)
print3("isnan", b1, b2, b3)
// CHECK-NEXT: isnan false false isnan

i1 = signbit(dx)
i2 = signbit(fx)
i3 = signbit(gx)
print3("signbit", i1, i2, i3)
// CHECK-NEXT: signbit 0 0 signbit

(d1, d2) = modf(dy)
(f1, f2) = modf(fy)
(g1, g2) = modf(gy)
print6("modf", d1,d2, f1,f2, g1,g2)
// CHECK-NEXT: modf 2.0,0.2 2.0,0.2 modf

d1 = ldexp(dx, ix)
f1 = ldexp(fx, ix)
g1 = ldexp(gx, ix)
print3("ldexp", d1, f1, g1)
// CHECK-NEXT: ldexp 204.8 204.8 ldexp

(d1, i1) = frexp(dy)
(f1, i2) = frexp(fy)
(g1, i3) = frexp(gy)
print6("frexp", d1,i1, f1,i2, g1,i3)
// CHECK-NEXT: frexp 0.55,2 0.55,2 frexp

i1 = ilogb(dy)
i2 = ilogb(fy)
i3 = ilogb(gy)
print3("ilogb", i1, i2, i3)
// CHECK-NEXT: ilogb 1 1 ilogb

d1 = scalbn(dx, ix)
f1 = scalbn(fx, ix)
g1 = scalbn(gx, ix)
print3("scalbn", d1, f1, g1)
// CHECK-NEXT: scalbn 204.8 204.8 scalbn

(d1, i1) = lgamma(dx)
(f1, i2) = lgamma(fx)
(g1, i3) = lgamma(gx)
print6("lgamma", d1,i1, f1,i2, g1,i3)
// CHECK-NEXT: lgamma 2.25271265173421,1 2.25271,1 lgamma

(d1, i1) = remquo(dz, dy)
(f1, i2) = remquo(fz, fy)
(g1, i3) = remquo(gz, gy)
print6("remquo", d1,i1, f1,i2, g1,i3)
// CHECK-NEXT: remquo 1.1,1 1.1,1 remquo

d1 = nan("12345")
f1 = nan("12345")
g1 = nan("12345")
print3("nan", d1, f1, g1)
// CHECK-NEXT: nan nan nan nan

d1 = fma(dx, dy, dz)
f1 = fma(fx, fy, fz)
g1 = fma(gx, gy, gz)
print3("fma", d1, f1, g1)
// CHECK-NEXT: fma 3.52 3.52 fma


d1 = j0(dx)
print("j0 \(d1) j0")
// CHECK-NEXT: j0 0.99750156206604 j0

d1 = j1(dx)
print("j1 \(d1) j1")
// CHECK-NEXT: j1 0.049937526036242 j1

d1 = jn(ix, dx)
print("jn \(d1) jn")
// CHECK-NEXT: jn 1.22299266103565e-22 jn

d1 = y0(dx)
print("y0 \(d1) y0")
// CHECK-NEXT: y0 -1.53423865135037 y0

d1 = y1(dx)
print("y1 \(d1) y1")
// CHECK-NEXT: y1 -6.45895109470203 y1

d1 = yn(ix, dx)
print("yn \(d1) yn")
// CHECK-NEXT: yn -2.36620129448696e+20 yn

