// RUN: %target-run-simple-swift | FileCheck %s

import Darwin

// inputs
let dx = Double(0.1)
let dy = Double(2.2)
let dz = Double(3.3)

let fx = Float(0.1)
let fy = Float(2.2)
let fz = Float(3.3)

let ix = Int(11)

// outputs
var d1, d2: Double
var f1, f2: Float
var i1, i2: Int

// The order of these tests matches tgmath.swift.gyb.

// Unary functions

d1 = acos(dx)
f1 = acos(fx)
println("acos \(d1) \(f1) acos")
// CHECK: acos 1.47062890563334 1.47062885761261 acos

d1 = asin(dx)
f1 = asin(fx)
println("asin \(d1) \(f1) asin")
// CHECK: asin 0.10016742116156 0.10016742348671 asin

d1 = atan(dx)
f1 = atan(fx)
println("atan \(d1) \(f1) atan")
// CHECK: atan 0.099668652491162 0.0996686518192291 atan

d1 = acosh(dx)
f1 = acosh(fx)
println("acosh \(d1) \(f1) acosh")
// CHECK: acosh nan nan acosh

d1 = asinh(dx)
f1 = asinh(fx)
println("asinh \(d1) \(f1) asinh")
// CHECK: asinh 0.0998340788992076 0.0998340770602226 asinh

d1 = atanh(dx)
f1 = atanh(fx)
println("atanh \(d1) \(f1) atanh")
// CHECK: atanh 0.100335347731076 0.100335352122784 atanh


d1 = cos(dx)
f1 = cos(fx)
println("cos \(d1) \(f1) cos")
// CHECK: cos 0.995004165278026 0.995004177093506 cos

d1 = sin(dx)
f1 = sin(fx)
println("sin \(d1) \(f1) sin")
// CHECK: sin 0.0998334166468282 0.0998334214091301 sin

d1 = tan(dx)
f1 = tan(fx)
println("tan \(d1) \(f1) tan")
// CHECK: tan 0.100334672085451 0.100334674119949 tan

d1 = cosh(dx)
f1 = cosh(fx)
println("cosh \(d1) \(f1) cosh")
// CHECK: cosh 1.0050041680558 1.00500416755676 cosh

d1 = sinh(dx)
f1 = sinh(fx)
println("sinh \(d1) \(f1) sinh")
// CHECK: sinh 0.100166750019844 0.100166752934456 sinh

d1 = tanh(dx)
f1 = tanh(fx)
println("tanh \(d1) \(f1) tanh")
// CHECK: tanh 0.0996679946249558 0.0996679961681366 tanh


d1 = exp(dx)
f1 = exp(fx)
println("exp \(d1) \(f1) exp")
// CHECK: exp 1.10517091807565 1.1051709651947 exp

d1 = log(dx)
f1 = log(fx)
println("log \(d1) \(f1) log")
// CHECK: log -2.30258509299405 -2.30258512496948 log

d1 = sqrt(dx)
f1 = sqrt(fx)
println("sqrt \(d1) \(f1) sqrt")
// CHECK: sqrt 0.316227766016838 0.31622776389122 sqrt

d1 = fabs(dx)
f1 = fabs(fx)
println("fabs \(d1) \(f1) fabs")
// CHECK: fabs 0.1 0.100000001490116 fabs

d1 = cbrt(dx)
f1 = cbrt(fx)
println("cbrt \(d1) \(f1) cbrt")
// CHECK: cbrt 0.464158883361278 0.464158892631531 cbrt

d1 = ceil(dx)
f1 = ceil(fx)
println("ceil \(d1) \(f1) ceil")
// CHECK: ceil 1.0 1.0 ceil


d1 = erf(dx)
f1 = erf(fx)
println("erf \(d1) \(f1) erf")
// CHECK: erf 0.112462916018285 0.112462915480137 erf

d1 = erfc(dx)
f1 = erfc(fx)
println("erfc \(d1) \(f1) erfc")
// CHECK: erfc 0.887537083981715 0.887537062168121 erfc

d1 = exp2(dx)
f1 = exp2(fx)
println("exp2 \(d1) \(f1) exp2")
// CHECK: exp2 1.07177346253629 1.07177340984344 exp2

d1 = expm1(dx)
f1 = expm1(fx)
println("expm1 \(d1) \(f1) expm1")
// CHECK: expm1 0.105170918075648 0.105170920491219 expm1

d1 = floor(dx)
f1 = floor(fx)
println("floor \(d1) \(f1) floor")
// CHECK: floor 0.0 0.0 floor

d1 = lgamma(dx)
f1 = lgamma(fx)
println("lgamma \(d1) \(f1) lgamma")
// CHECK: lgamma 2.25271265173421 2.25271272659302 lgamma


d1 = log10(dx)
f1 = log10(fx)
println("log10 \(d1) \(f1) log10")
// CHECK: log10 -1.0 -1.0 log10

d1 = log1p(dx)
f1 = log1p(fx)
println("log1p \(d1) \(f1) log1p")
// CHECK: log1p 0.0953101798043249 0.0953101813793182 log1p

d1 = log2(dx)
f1 = log2(fx)
println("log2 \(d1) \(f1) log2")
// CHECK: log2 -3.32192809488736 -3.32192802429199 log2

d1 = logb(dx)
f1 = logb(fx)
println("logb \(d1) \(f1) logb")
// CHECK: logb -4.0 -4.0 logb


d1 = nearbyint(dx)
f1 = nearbyint(fx)
println("nearbyint \(d1) \(f1) nearbyint")
// CHECK: nearbyint 0.0 0.0 nearbyint

d1 = rint(dx)
f1 = rint(fx)
println("rint \(d1) \(f1) rint")
// CHECK: rint 0.0 0.0 rint

d1 = round(dx)
f1 = round(fx)
println("round \(d1) \(f1) round")
// CHECK: round 0.0 0.0 round

d1 = tgamma(dx)
f1 = tgamma(fx)
println("tgamma \(d1) \(f1) tgamma")
// CHECK: tgamma 9.51350769866873 9.51350784301758 tgamma

d1 = trunc(dx)
f1 = trunc(fx)
println("trunc \(d1) \(f1) trunc")
// CHECK: trunc 0.0 0.0 trunc


d1 = pow(dx, dy)
f1 = pow(fx, fy)
println("pow \(d1) \(f1) pow")
// CHECK: pow 0.00630957344480193 0.00630957307294011 pow

d1 = atan2(dx, dy)
f1 = atan2(fx, fy)
println("atan2 \(d1) \(f1) atan2")
// CHECK: atan2 0.045423279421577 0.0454232804477215 atan2

d1 = copysign(dx, dy)
f1 = copysign(fx, fy)
println("copysign \(d1) \(f1) copysign")
// CHECK: copysign 0.1 0.100000001490116 copysign

d1 = fdim(dx, dy)
f1 = fdim(fx, fy)
println("fdim \(d1) \(f1) fdim")
// CHECK: fdim 0.0 0.0 fdim

d1 = fmax(dx, dy)
f1 = fmax(fx, fy)
println("fmax \(d1) \(f1) fmax")
// CHECK: fmax 2.2 2.20000004768372 fmax

d1 = fmin(dx, dy)
f1 = fmin(fx, fy)
println("fmin \(d1) \(f1) fmin")
// CHECK: fmin 0.1 0.100000001490116 fmin

d1 = fmod(dx, dy)
// FIXME: rdar://17275152 fmod overlay busted
// f1 = fmod(fx, fy)
f1 = 0
println("fmod \(d1) \(f1) fmod")
// CHECK: fmod 0.1 0.0 fmod

d1 = hypot(dx, dy)
f1 = hypot(fx, fy)
println("hypot \(d1) \(f1) hypot")
// CHECK: hypot 2.20227155455452 2.2022716999054 hypot

d1 = nextafter(dx, dy)
f1 = nextafter(fx, fy)
println("nextafter \(d1) \(f1) nextafter")
// CHECK: nextafter 0.1 0.100000008940697 nextafter

d1 = remainder(dx, dy)
f1 = remainder(fx, fy)
println("remainder \(d1) \(f1) remainder")
// CHECK: remainder 0.1 0.100000001490116 remainder


d1 = fma(dx, dy, dz)
f1 = fma(fx, fy, fz)
println("fma \(d1) \(f1) fma")
// CHECK: fma 3.52 3.51999998092651 fma

(d1, i1) = frexp(dy)
(f1, i2) = frexp(fy)
println("frexp \(d1),\(i1) \(f1),\(i2) frexp")
// CHECK: frexp 0.55,2 0.550000011920929,2 frexp

i1 = ilogb(dy)
i2 = ilogb(fy)
println("ilogb \(i1) \(i2) ilogb")
// CHECK: ilogb 1 1 ilogb

d1 = ldexp(dx, ix)
f1 = ldexp(fx, ix)
println("ldexp \(d1) \(f1) ldexp")
// CHECK: ldexp 204.8 204.800003051758 ldexp

(d1, i1) = remquo(dz, dy)
(f1, i2) = remquo(fz, fy)
println("remquo \(d1),\(i1) \(f1),\(i2) remquo")
// CHECK: remquo 1.1,1 1.09999990463257,1 remquo

d1 = scalbn(dx, ix)
f1 = scalbn(fx, ix)
println("scalbn \(d1) \(f1) scalbn")
// CHECK: scalbn 204.8 204.800003051758 scalbn

// FIXME: unhandled: 
// llrint llround lrint lround nexttoward scalbln 
// carg cimag conj cproj creal
