//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A representation of high precision time.
///
/// `Duration` represents an elapsed time value with high precision in an 
/// integral form. It may be used for measurements of varying clock sources. In 
/// those cases it represents the elapsed time measured by that clock. 
/// Calculations using `Duration` may span from a negative value to a positive 
/// value and have a suitable range to at least cover attosecond scale for both
/// small elapsed durations like sub-second precision to durations that span
/// centuries.
///
/// Typical construction of `Duration` values should be created via the
/// static methods for specific time values. 
///
///      var d: Duration = .seconds(3)
///      d += .milliseconds(33)
///      print(d) // 3.033 seconds
///
/// `Duration` itself does not ferry any additional information other than the 
/// temporal measurement component; specifically leap seconds should be 
/// represented as an additional accessor since that is specific only to certain
/// clock implementations.
@available(SwiftStdlib 5.0, *)
@backDeployed(before: SwiftStdlib 5.7)
@frozen
public struct Duration: Sendable {
  /// The low 64 bits of a 128-bit signed integer value counting attoseconds.
  public var _low: UInt64

  /// The high 64 bits of a 128-bit signed integer value counting attoseconds.
  public var _high: Int64

  @inlinable
  public init(_high: Int64, low: UInt64) {
    self._low = low
    self._high = _high
  }

  /// Construct a `Duration` by adding attoseconds to a seconds value.
  ///
  /// This is useful for when an external decomposed components of a `Duration`
  /// has been stored and needs to be reconstituted. Since the values are added
  /// no precondition is expressed for the attoseconds being limited to 1e18.
  ///
  ///       let d1 = Duration(
  ///         secondsComponent: 3, 
  ///         attosecondsComponent: 123000000000000000)
  ///       print(d1) // 3.123 seconds
  ///
  ///       let d2 = Duration(
  ///         secondsComponent: 3, 
  ///         attosecondsComponent: -123000000000000000)
  ///       print(d2) // 2.877 seconds
  ///
  ///       let d3 = Duration(
  ///         secondsComponent: -3, 
  ///         attosecondsComponent: -123000000000000000)
  ///       print(d3) // -3.123 seconds
  ///
  /// - Parameters:
  ///   - secondsComponent: The seconds component portion of the `Duration` 
  ///                       value.
  ///   - attosecondsComponent: The attosecond component portion of the 
  ///                           `Duration` value.
  public init(secondsComponent: Int64, attosecondsComponent: Int64) {
    self = Duration.seconds(secondsComponent) +
           Duration(attoseconds: Int128(attosecondsComponent))
  }
}

extension Duration {
  /// The composite components of the `Duration`.
  ///
  /// This is intended for facilitating conversions to existing time types. The
  /// attoseconds value will not exceed 1e18 or be lower than -1e18.
  public var components: (seconds: Int64, attoseconds: Int64) {
    let (seconds, attoseconds) = attoseconds.dividedBy1e18()
    return (Int64(seconds), Int64(attoseconds))
  }
}

@available(SwiftStdlib 5.0, *)
extension Duration {
  /// The number of attoseconds represented by this `Duration`.
  ///
  /// This property provides direct access to the underlying number of attoseconds 
  /// that the current `Duration` represents.
  ///
  ///     let d = Duration.seconds(1)
  ///     print(d.attoseconds) // 1_000_000_000_000_000_000
  @available(SwiftStdlib 5.0, *)
  @export(implementation)
  public var attoseconds: Int128 {
    Int128(_low: _low, _high: _high)
  }
  
  /// Construct a `Duration` from the given number of attoseconds.
  ///
  /// This directly constructs a `Duration` from the given number of attoseconds.
  ///
  ///     let d = Duration(attoseconds: 1_000_000_000_000_000_000)
  ///     print(d) // 1.0 seconds
  ///
  /// - Parameter attoseconds: The total duration expressed in attoseconds.
  @available(SwiftStdlib 5.0, *)
  @export(implementation)
  public init(attoseconds: Int128) {
    self.init(_high: attoseconds._high, low: attoseconds._low)
  }
}

extension Duration {
  /// Construct a `Duration` given a number of seconds represented as a 
  /// `BinaryInteger`.
  ///
  ///       let d: Duration = .seconds(77)
  ///
  /// - Returns: A `Duration` representing a given number of seconds.
  @inlinable
  public static func seconds<T: BinaryInteger>(_ seconds: T) -> Duration {
    guard let high = Int64(exactly: seconds >> 64) else { fatalError() }
    let low = UInt64(truncatingIfNeeded: seconds)
    let lowScaled = low.multipliedFullWidth(by: 1_000_000_000_000_000_000)
    let highScaled = high * 1_000_000_000_000_000_000
    return Duration(_high: highScaled + Int64(lowScaled.high), low: lowScaled.low)
  }
  
  /// Construct a `Duration` given a duration and scale, taking care so that
  /// exact integer durations are preserved exactly.
  @_alwaysEmitIntoClient
  internal init(_ duration: Double, scale: UInt64) {
    // Split the duration into integral and fractional parts, as we need to
    // handle them slightly differently to ensure that integer values are
    // never rounded if `scale` is representable as Double.
    let integralPart = duration.rounded(.towardZero)
    let fractionalPart = duration - integralPart
    self.init(attoseconds:
      // This term may trap due to overflow, but it cannot round, so if the
      // input `seconds` is an exact integer, we get an exact integer result.
      Int128(integralPart).multiplied(by: scale) +
      // This term may round, but cannot overflow.
      Int128((fractionalPart * Double(scale)).rounded())
    )
  }

  /// Construct a `Duration` given a number of seconds represented as a 
  /// `Double` by converting the value into the closest attosecond scale value.
  ///
  ///       let d: Duration = .seconds(22.93)
  ///
  /// - Returns: A `Duration` representing a given number of seconds.
  public static func seconds(_ seconds: Double) -> Duration {
    Duration(seconds, scale: 1_000_000_000_000_000_000)
  }

  /// Construct a `Duration` given a number of milliseconds represented as a 
  /// `BinaryInteger`.
  ///
  ///       let d: Duration = .milliseconds(645)
  ///
  /// - Returns: A `Duration` representing a given number of milliseconds.
  @inlinable
  public static func milliseconds<T: BinaryInteger>(
    _ milliseconds: T
  ) -> Duration {
    guard let high = Int64(exactly: milliseconds >> 64) else { fatalError() }
    let low = UInt64(truncatingIfNeeded: milliseconds)
    let lowScaled = low.multipliedFullWidth(by: 1_000_000_000_000_000)
    let highScaled = high * 1_000_000_000_000_000
    return Duration(_high: highScaled + Int64(lowScaled.high), low: lowScaled.low)
  }

  /// Construct a `Duration` given a number of seconds milliseconds as a 
  /// `Double` by converting the value into the closest attosecond scale value.
  ///
  ///       let d: Duration = .milliseconds(88.3)
  ///
  /// - Returns: A `Duration` representing a given number of milliseconds.
  public static func milliseconds(_ milliseconds: Double) -> Duration {
    Duration(milliseconds, scale: 1_000_000_000_000_000)
  }

  /// Construct a `Duration` given a number of microseconds represented as a 
  /// `BinaryInteger`.
  ///
  ///       let d: Duration = .microseconds(12)
  ///
  /// - Returns: A `Duration` representing a given number of microseconds.
  @inlinable
  public static func microseconds<T: BinaryInteger>(
    _ microseconds: T
  ) -> Duration {
    guard let high = Int64(exactly: microseconds >> 64) else { fatalError() }
    let low = UInt64(truncatingIfNeeded: microseconds)
    let lowScaled = low.multipliedFullWidth(by: 1_000_000_000_000)
    let highScaled = high * 1_000_000_000_000
    return Duration(_high: highScaled + Int64(lowScaled.high), low: lowScaled.low)
  }

  /// Construct a `Duration` given a number of seconds microseconds as a 
  /// `Double` by converting the value into the closest attosecond scale value.
  ///
  ///       let d: Duration = .microseconds(382.9)
  ///
  /// - Returns: A `Duration` representing a given number of microseconds.
  public static func microseconds(_ microseconds: Double) -> Duration {
    Duration(microseconds, scale: 1_000_000_000_000)
  }

  /// Construct a `Duration` given a number of nanoseconds represented as a 
  /// `BinaryInteger`.
  ///
  ///       let d: Duration = .nanoseconds(1929)
  ///
  /// - Returns: A `Duration` representing a given number of nanoseconds.
  @inlinable
  public static func nanoseconds<T: BinaryInteger>(
    _ nanoseconds: T
  ) -> Duration {
    guard let high = Int64(exactly: nanoseconds >> 64) else { fatalError() }
    let low = UInt64(truncatingIfNeeded: nanoseconds)
    let lowScaled = low.multipliedFullWidth(by: 1_000_000_000)
    let highScaled = high * 1_000_000_000
    return Duration(_high: highScaled + Int64(lowScaled.high), low: lowScaled.low)
  }
  
  /// Construct a `Duration` given a number of nanoseconds as a
  /// `Double` by converting the value into the closest attosecond scale value.
  ///
  ///       let d: Duration = .nanoseconds(382.9)
  ///
  /// - Returns: A `Duration` representing a given number of nanoseconds.
  @available(StdlibDeploymentTarget 6.2, *)
  public static func nanoseconds(_ nanoseconds: Double) -> Duration {
    Duration(nanoseconds, scale: 1_000_000_000)
  }
}

@_unavailableInEmbedded
extension Duration: Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let high = try container.decode(Int64.self)
    let low = try container.decode(UInt64.self)
    self.init(_high: high, low: low)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(_high)
    try container.encode(_low)
  }
}

extension Duration: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(attoseconds)
  }
}

extension Duration: Equatable {
  public static func == (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs.attoseconds == rhs.attoseconds
  }
}

extension Duration: Comparable {
  public static func < (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs.attoseconds < rhs.attoseconds
  }
}

extension Duration: AdditiveArithmetic {
  public static var zero: Duration { Duration(attoseconds: 0) }

  public static func + (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(attoseconds: lhs.attoseconds + rhs.attoseconds)
  }

  public static func - (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(attoseconds: lhs.attoseconds - rhs.attoseconds)
  }

  public static func += (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs + rhs
  }

  public static func -= (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs - rhs
  }
}

extension Duration {
  public static func / (_ lhs: Duration, _ rhs: Double) -> Duration {
    Duration(attoseconds: Int128(Double(lhs.attoseconds) / rhs))
  }

  public static func /= (_ lhs: inout Duration, _ rhs: Double) {
    lhs = lhs / rhs
  }

  public static func / <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(attoseconds: lhs.attoseconds / Int128(rhs))
  }

  public static func /= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs / rhs
  }

  public static func / (_ lhs: Duration, _ rhs: Duration) -> Double {
    Double(lhs.attoseconds) / Double(rhs.attoseconds)
  }

  public static func * (_ lhs: Duration, _ rhs: Double) -> Duration {
    Duration(attoseconds: Int128(Double(lhs.attoseconds) * rhs))
  }

  public static func * <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(attoseconds: lhs.attoseconds * Int128(rhs))
  }

  public static func *= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs * rhs
  }
}

@_unavailableInEmbedded
extension Duration: CustomStringConvertible {
  public var description: String {
    return (Double(attoseconds) / 1e18).description + " seconds"
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: DurationProtocol { }

extension Duration: ConvertibleToBytes, ConvertibleFromBytes {}

@available(SwiftStdlib 5.0, *)
extension Int128 {
  @_alwaysEmitIntoClient
  internal func multiplied(by other: UInt64) -> Self {
    // Mathematically this is just self * Int128(other), but the optimizer
    // doesn't quite get all the invariants it needs to optimize that as well
    // as we can.
    let tail = _low.multipliedFullWidth(by: other)
    let wide = (self &>> 64) &* Int128(other) &+ Int128(tail.high)
    let head = Int64(bitPattern: wide._low)
    precondition(head &>> 63 == Int64(wide._high))
    return Int128(_low: tail.low, _high: head)
  }
  
  @_alwaysEmitIntoClient
  internal func dividedBy1e18() -> (quotient: Self, remainder: Self) {
    let m = Int128(_low: 8336148766501648893, _high: 664613997892457936)
    var q = self.multipliedFullWidth(by: m).high
    q &>>= 55
    q &+= Int128(bitPattern: UInt128(bitPattern: self) &>> 127)
    let r = self &- q &* (1000000000000000000 as Int128)
    return (q, r)
  }
}
