//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@exported import CoreGraphics
import Darwin

public struct CGFloat {
#if arch(i386) || arch(arm)
  /// The native type used to store the CGFloat, which is Float on
  /// 32-bit architectures and Double on 64-bit architectures.
  public typealias NativeType = Float
#elseif arch(x86_64) || arch(arm64)
  /// The native type used to store the CGFloat, which is Float on
  /// 32-bit architectures and Double on 64-bit architectures.
  public typealias NativeType = Double
#endif

  @transparent public init() {
    self.native = 0.0
  }

  @transparent public init(_ value: Int) {
    self.native = NativeType(value)
  }

  @transparent public init(_ value: UInt) {
    self.native = NativeType(value)
  }

  @transparent public init(_ value: Float) {
    self.native = NativeType(value)
  }

  @transparent public init(_ value: Double) {
    self.native = NativeType(value)
  }

  /// The native value.
  public var native: NativeType
}

@transparent extension CGFloat : FloatingPointNumber {
  public typealias _BitsType = UInt

  public static func _fromBitPattern(bits: UInt) -> CGFloat {
    return CGFloat(
             NativeType._fromBitPattern(NativeType._BitsType(bits)))
  }

  public func _toBitPattern() -> UInt {
    return UInt(native._toBitPattern())
  }

  public static var infinity: CGFloat {
    return CGFloat(NativeType.infinity)
  }

  public static var NaN: CGFloat {
    return CGFloat(NativeType.NaN)
  }

  public static var quietNaN: CGFloat {
    return CGFloat(NativeType.quietNaN)
  }

  public var isSignMinus: Bool {
    return native.isSignMinus
  }

  public var isNormal:  Bool {
    return native.isNormal
  }

  public var isFinite:  Bool {
    return native.isFinite
  }

  public var isZero:  Bool {
    return native.isZero
  }

  public var isSubnormal:  Bool {
    return native.isSubnormal
  }

  public var isInfinite:  Bool {
    return native.isInfinite
  }

  public var isNaN:  Bool {
    return native.isNaN
  }

  public var isSignaling: Bool {
    return native.isSignaling
  }

  public var floatingPointClass: FloatingPointClassification {
    return native.floatingPointClass
  }
}

extension CGFloat {
  @transparent public static var min: CGFloat {
#if arch(i386) || arch(arm)
   return CGFloat(FLT_MIN)
#else
   return CGFloat(DBL_MIN)
#endif
  }

  @transparent public static var max: CGFloat {
#if arch(i386) || arch(arm)
   return CGFloat(FLT_MAX)
#else
   return CGFloat(DBL_MAX)
#endif
  }
}

@availability(*,unavailable,message="use CGFloat.min")
public let CGFLOAT_MIN = CGFloat.min

@availability(*,unavailable,message="use CGFloat.max")
public let CGFLOAT_MAX = CGFloat.max

@transparent extension CGFloat : Printable {
  public var description: String {
    return native.description
  }
}

@transparent extension CGFloat : Hashable {
  public var hashValue: Int {
    return native.hashValue
  }
}

@transparent extension CGFloat : FloatLiteralConvertible {
  public 
  static func convertFromFloatLiteral(value: NativeType) -> CGFloat {
    return CGFloat(value)
  }
}

@transparent extension CGFloat : IntegerLiteralConvertible {
  public
  static func convertFromIntegerLiteral(value: Int) -> CGFloat {
    return CGFloat(value)
  }
}

@transparent extension CGFloat : AbsoluteValuable {
  @transparent
  public static func abs(x: CGFloat) -> CGFloat {
    return CGFloat(NativeType.abs(x.native))
  }
}

@transparent extension Int {
  public init(_ value: CGFloat) { 
    self = Int(value.native) 
  }
}

@transparent extension Double {
  public init(_ value: CGFloat) { 
    self = Double(value.native) 
  }
}

@transparent extension Float {
  public init(_ value: CGFloat) { 
    self = Float(value.native) 
  }
}

// Comparisons.
@transparent extension CGFloat : Comparable { }

@transparent public
func ==(lhs: CGFloat, rhs: CGFloat) -> Bool {
  return lhs.native == rhs.native
}

@transparent public
func <(lhs: CGFloat, rhs: CGFloat) -> Bool {
  return lhs.native < rhs.native
}

@transparent extension CGFloat : RandomAccessIndex {
  @transparent public
  func successor() -> CGFloat {
    return CGFloat(self.native + 1.0)
  }
  @transparent public
  func predecessor() -> CGFloat {
    return CGFloat(self.native - 1.0)
  }
  
  @transparent public
  func distanceTo(other: CGFloat) -> CGFloat.DistanceType {
    return Int(other.native-self.native)
  }
  
  @transparent public
  func advancedBy(amount: CGFloat.DistanceType) -> CGFloat {
    return CGFloat(self.native + CGFloat(amount).native)
  }
}

// CGFloat unary operators
@transparent @prefix public
func + (x: CGFloat) -> CGFloat { return x }

@transparent @prefix public
func - (x: CGFloat) -> CGFloat { return CGFloat(-x.native) }

@transparent @prefix @assignment public
func ++ (inout x: CGFloat) -> CGFloat { 
  x.native += 1.0
  return x
}

@transparent @prefix @assignment public
func -- (inout x: CGFloat) -> CGFloat { 
  x.native -= 1.0
  return x
}

@transparent @postfix @assignment public
func ++ (inout x: CGFloat) -> CGFloat { 
  let tmp = x
  x.native += 1.0
  return tmp 
}

@transparent @postfix @assignment public
func -- (inout x: CGFloat) -> CGFloat { 
  let tmp = x
  x.native -= 1.0
  return tmp 
}

// CGFloat arithmetic
@transparent public
func +(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(lhs.native + rhs.native)
}

@transparent public
func -(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(lhs.native - rhs.native)
}

@transparent public
func *(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(lhs.native * rhs.native)
}

@transparent public
func /(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(lhs.native / rhs.native)
}

@transparent public
func %(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(lhs.native % rhs.native)
}

// CGFloat assignment operators.
@transparent @assignment public
func +=(inout lhs: CGFloat, rhs: CGFloat) {
  lhs.native = lhs.native + rhs.native
}

@transparent @assignment public
func -=(inout lhs: CGFloat, rhs: CGFloat) {
  lhs.native = lhs.native - rhs.native
}

@transparent @assignment public
func *=(inout lhs: CGFloat, rhs: CGFloat) {
  lhs.native = lhs.native * rhs.native
}

@transparent @assignment public
func /=(inout lhs: CGFloat, rhs: CGFloat) {
  lhs.native = lhs.native / rhs.native
}

@transparent @assignment public
func %=(inout lhs: CGFloat, rhs: CGFloat) {
  lhs.native = lhs.native % rhs.native
}

// CGFloat tgmath.
@transparent public
func acos(x: CGFloat) -> CGFloat {
  return CGFloat(acos(x.native))
}

@transparent public
func cos(x: CGFloat) -> CGFloat {
  return CGFloat(cos(x.native))
}

@transparent public
func sin(x: CGFloat) -> CGFloat {
  return CGFloat(sin(x.native))
}

@transparent public
func asin(x: CGFloat) -> CGFloat {
  return CGFloat(asin(x.native))
}

@transparent public
func atan(x: CGFloat) -> CGFloat {
  return CGFloat(atan(x.native))
}

@transparent public
func tan(x: CGFloat) -> CGFloat {
  return CGFloat(tan(x.native))
}

@transparent public
func acosh(x: CGFloat) -> CGFloat {
  return CGFloat(acosh(x.native))
}

@transparent public
func asinh(x: CGFloat) -> CGFloat {
  return CGFloat(asinh(x.native))
}

@transparent public
func atanh(x: CGFloat) -> CGFloat {
  return CGFloat(atanh(x.native))
}

@transparent public
func cosh(x: CGFloat) -> CGFloat {
  return CGFloat(cosh(x.native))
}

@transparent public
func sinh(x: CGFloat) -> CGFloat {
  return CGFloat(sinh(x.native))
}

@transparent public
func tanh(x: CGFloat) -> CGFloat {
  return CGFloat(tanh(x.native))
}

@transparent public
func exp(x: CGFloat) -> CGFloat {
  return CGFloat(exp(x.native))
}

@transparent public
func exp2(x: CGFloat) -> CGFloat {
  return CGFloat(exp2(x.native))
}

@transparent public
func expm1(x: CGFloat) -> CGFloat {
  return CGFloat(expm1(x.native))
}

@transparent public
func log(x: CGFloat) -> CGFloat {
  return CGFloat(log(x.native))
}

@transparent public
func log10(x: CGFloat) -> CGFloat {
  return CGFloat(log10(x.native))
}

@transparent public
func log2(x: CGFloat) -> CGFloat {
  return CGFloat(log2(x.native))
}

@transparent public
func log1p(x: CGFloat) -> CGFloat {
  return CGFloat(log1p(x.native))
}

@transparent public
func logb(x: CGFloat) -> CGFloat {
  return CGFloat(logb(x.native))
}

@transparent public
func cbrt(x: CGFloat) -> CGFloat {
  return CGFloat(cbrt(x.native))
}

@transparent public
func erf(x: CGFloat) -> CGFloat {
  return CGFloat(erf(x.native))
}

@transparent public
func erfc(x: CGFloat) -> CGFloat {
  return CGFloat(erfc(x.native))
}

@transparent public
func tgamma(x: CGFloat) -> CGFloat {
  return CGFloat(tgamma(x.native))
}

@transparent public
func fabs(x: CGFloat) -> CGFloat {
  return CGFloat(fabs(x.native))
}

@transparent public
func sqrt(x: CGFloat) -> CGFloat {
  return CGFloat(sqrt(x.native))
}

@transparent public
func ceil(x: CGFloat) -> CGFloat {
  return CGFloat(ceil(x.native))
}

@transparent public
func floor(x: CGFloat) -> CGFloat {
  return CGFloat(floor(x.native))
}

@transparent public
func nearbyint(x: CGFloat) -> CGFloat {
  return CGFloat(nearbyint(x.native))
}

@transparent public
func rint(x: CGFloat) -> CGFloat {
  return CGFloat(rint(x.native))
}

@transparent public
func round(x: CGFloat) -> CGFloat {
  return CGFloat(round(x.native))
}

@transparent public
func trunc(x: CGFloat) -> CGFloat {
  return CGFloat(trunc(x.native))
}

@transparent public
func atan2(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(atan2(lhs.native, rhs.native))
}

@transparent public
func hypot(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(hypot(lhs.native, rhs.native))
}

@transparent public
func pow(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(pow(lhs.native, rhs.native))
}

// FIXME: fmod

@transparent public
func remainder(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(remainder(lhs.native, rhs.native))
}

@transparent public
func copysign(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(copysign(lhs.native, rhs.native))
}

@transparent public
func nextafter(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(nextafter(lhs.native, rhs.native))
}

@transparent public
func fdim(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(fdim(lhs.native, rhs.native))
}

@transparent public
func fmax(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(fmax(lhs.native, rhs.native))
}

@transparent public
func fmin(lhs: CGFloat, rhs: CGFloat) -> CGFloat {
  return CGFloat(fmin(lhs.native, rhs.native))
}

@transparent public
func fpclassify(x: CGFloat) -> Int {
  return fpclassify(x.native)
}

@transparent public
func isnormal(x: CGFloat) -> Bool {
  return isnormal(x.native)
}

@transparent public
func isfinite(x: CGFloat) -> Bool {
  return isfinite(x.native)
}

@transparent public
func isinf(x: CGFloat) -> Bool {
  return isinf(x.native)
}

@transparent public
func isnan(x: CGFloat) -> Bool {
  return isnan(x.native)
}

@transparent public
func signbit(x: CGFloat) -> Int {
  return signbit(x.native)
}

@transparent public
func modf(x: CGFloat) -> (CGFloat, CGFloat) {
  let (ipart, fpart) = modf(x.native)
  return (CGFloat(ipart), CGFloat(fpart))
}

@transparent public
func ldexp(x: CGFloat, n: Int) -> CGFloat {
  return CGFloat(ldexp(x.native, n))
}

@transparent public
func frexp(x: CGFloat) -> (CGFloat, Int) {
  let (frac, exp) = frexp(x.native)
  return (CGFloat(frac), exp)
}

@transparent public
func ilogb(x: CGFloat) -> Int {
  return ilogb(x.native)
}

@transparent public
func scalbn(x: CGFloat, n: Int) -> CGFloat {
  return CGFloat(scalbn(x.native, n))
}

@transparent public
func lgamma(x: CGFloat) -> (CGFloat, Int) {
  let (value, sign) = lgamma(x.native)
  return (CGFloat(value), sign)
}

@transparent public
func remquo(x: CGFloat, y: CGFloat) -> (CGFloat, Int) {
  let (rem, quo) = remquo(x.native, y.native)
  return (CGFloat(rem), quo)
}

@transparent public
func nan(tag: String) -> CGFloat {
  return CGFloat(nan(tag) as CGFloat.NativeType)
}

@transparent public
func fma(x: CGFloat, y: CGFloat, z: CGFloat) -> CGFloat {
  return CGFloat(fma(x.native, y.native, z.native))
}

@transparent public
func j0(x: CGFloat) -> CGFloat {
  return CGFloat(j0(Double(x.native)))
}

@transparent public
func j1(x: CGFloat) -> CGFloat {
  return CGFloat(j1(Double(x.native)))
}

@transparent public
func jn(n: Int, x: CGFloat) -> CGFloat {
  return CGFloat(jn(n, Double(x.native)))
}

@transparent public
func y0(x: CGFloat) -> CGFloat {
  return CGFloat(y0(Double(x.native)))
}

@transparent public
func y1(x: CGFloat) -> CGFloat {
  return CGFloat(y1(Double(x.native)))
}

@transparent public
func yn(n: Int, x: CGFloat) -> CGFloat {
  return CGFloat(yn(n, Double(x.native)))
}
