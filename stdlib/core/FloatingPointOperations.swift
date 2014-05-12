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

enum FloatingPointClassification {
  case SignalingNaN
  case QuietNaN
  case NegativeInfinity
  case NegativeNormal
  case NegativeSubnormal
  case NegativeZero
  case PositiveZero
  case PositiveSubnormal
  case PositiveNormal
  case PositiveInfinity
}


extension FloatingPointClassification : Equatable {}
func ==(lhs: FloatingPointClassification, rhs: FloatingPointClassification) -> Bool {
  switch (lhs, rhs) {
  case (.SignalingNaN, .SignalingNaN),
       (.QuietNaN, .QuietNaN),
       (.NegativeInfinity, .NegativeInfinity),
       (.NegativeNormal, .NegativeNormal),
       (.NegativeSubnormal, .NegativeSubnormal),
       (.NegativeZero, .NegativeZero),
       (.PositiveZero, .PositiveZero),
       (.PositiveSubnormal, .PositiveSubnormal),
       (.PositiveNormal, .PositiveNormal),
       (.PositiveInfinity, .PositiveInfinity):
    return true

  default:
    return false
  }
}


protocol FloatingPointNumber {
  typealias _BitsType
  class func _fromBitPattern(bits: _BitsType) -> Self
  func _toBitPattern() -> _BitsType

  // FIXME: make these readonly static properties.

  /// Returns positive infinity.
  class func inf() -> Self

  /// Returns a quiet NaN.
  class func NaN() -> Self

  class func quietNaN() -> Self

  /// @{
  /// IEEE 754-2008 Non-computational operations.

  // IEEE 754 calls this 'class', but this name is a keyword, and is too
  // general.
  var floatingPointClass: FloatingPointClassification { get }

  /// Returns true if this number has a negative sign.
  func isSignMinus() -> Bool

  /// Returns true if this number is normal (not zero, subnormal, infinite, or
  /// NaN).
  func isNormal() -> Bool

  /// Returns true if this number is zero, subnormal, or normal (not infinite
  /// or NaN).
  func isFinite() -> Bool

  /// Returns true if this number is +0.0 or -0.0.
  func isZero() -> Bool

  /// Returns true if this number is subnormal.
  func isSubnormal() -> Bool

  /// Returns true if this number is infinite.
  func isInfinite() -> Bool

  /// Returns true if this number is NaN.
  func isNaN() -> Bool

  /// Returns true if this number is a signaling NaN.
  func isSignaling() -> Bool

  // Not implemented, because it only makes sense for decimal floating point.
  // Binary floating point numbers are always canonical.
  // func isCanonical() -> Bool

  /// @}
}
