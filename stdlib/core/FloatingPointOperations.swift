enum IEEEFloatingPointClass {
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


extension IEEEFloatingPointClass : Equatable {}
func ==(lhs: IEEEFloatingPointClass, rhs: IEEEFloatingPointClass) -> Bool {
  switch (lhs, rhs) {
    case (.SignalingNaN, .SignalingNaN):
    case (.QuietNaN, .QuietNaN):
    case (.NegativeInfinity, .NegativeInfinity):
    case (.NegativeNormal, .NegativeNormal):
    case (.NegativeSubnormal, .NegativeSubnormal):
    case (.NegativeZero, .NegativeZero):
    case (.PositiveZero, .PositiveZero):
    case (.PositiveSubnormal, .PositiveSubnormal):
    case (.PositiveNormal, .PositiveNormal):
    case (.PositiveInfinity, .PositiveInfinity):
    return true

    default:
    return false
  }
}


protocol IEEEFloatingPointNumber {
  typealias _BitsType
  static func _fromBitPattern(bits: _BitsType) -> Self
  func _toBitPattern() -> _BitsType

  // FIXME: make these readonly static properties.

  /// Returns positive infinity.
  static func inf() -> Self

  /// Returns a quiet NaN.
  static func NaN() -> Self

  static func quietNaN() -> Self
  static func signalingNaN() -> Self

  /// @{
  /// IEEE 754-2008 Non-computational operations.

  // IEEE 754 calls this 'class', but this name is a keyword, and is too
  // general.
  // FIXME: make readonly.
  var floatingPointClass: IEEEFloatingPointClass

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
