//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@frozen
public struct Duration: Sendable {
  /// The low 64 bits of a 128-bit signed integer value counting attoseconds.
  @usableFromInline
  internal var _low: UInt64

  /// The high 64 bits of a 128-bit signed integer value counting attoseconds.
  @usableFromInline
  internal var _high: Int64

  internal init(_attoseconds: _Int128) {
    self._low = _attoseconds.low
    self._high = _attoseconds.high
  }

  internal init(_seconds: Int64, nanoseconds: Int64) {
    self = Duration.seconds(_seconds) + Duration.nanoseconds(nanoseconds)
  }

  internal var _attoseconds: _Int128 {
    _Int128(high: _high, low: _low)
  }

  public var seconds: Int64 {
    Int64(_attoseconds / 1_000_000_000_000_000_000)
  }

  public var nanoseconds: Int64 {
    let seconds = _attoseconds / 1_000_000_000_000_000_000
    let nanoseconds =
      Int64((_attoseconds - seconds * 1_000_000_000_000_000_000) / 1_000_000_000)
    return nanoseconds
  }
}

extension Duration {
  public static func seconds<T: BinaryInteger>(_ seconds: T) -> Duration {
    return Duration(_attoseconds: _Int128(seconds) *
                                 1_000_000_000_000_000_000)
  }

  public static func seconds(_ seconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(seconds *
                                 1_000_000_000_000_000_000))
  }

  public static func milliseconds<T: BinaryInteger>(
    _ milliseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(milliseconds) *
                                 1_000_000_000_000_000)
  }

  public static func milliseconds(_ milliseconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(milliseconds *
                                 1_000_000_000_000_000))
  }

  public static func microseconds<T: BinaryInteger>(
    _ microseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(microseconds) *
                                 1_000_000_000_000)
  }

  public static func microseconds(_ microseconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(microseconds *
                                 1_000_000_000_000))
  }

  public static func nanoseconds<T: BinaryInteger>(
    _ nanoseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(nanoseconds) *
                                 1_000_000_000)
  }
}

extension Duration: Codable {
  private enum CodingKeys: String, CodingKey {
    case seconds = "seconds"
    case nanoseconds = "nanoseconds"
  }

  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let seconds = try container.decode(Int64.self, forKey: .seconds)
    let nanoseconds = try container.decode(Int64.self, forKey: .nanoseconds)
    self.init(_seconds: seconds, nanoseconds: nanoseconds)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(seconds, forKey: .seconds)
    try container.encode(nanoseconds, forKey: .nanoseconds)
  }
}

extension Duration: Hashable { }

extension Duration: Equatable {
  public static func == (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs._attoseconds == rhs._attoseconds
  }
}

extension Duration: Comparable {
  public static func < (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs._attoseconds < rhs._attoseconds
  }
}

extension Duration: AdditiveArithmetic {
  public static var zero: Duration { Duration(_attoseconds: 0) }

  public static func + (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_attoseconds: lhs._attoseconds + rhs._attoseconds)
  }

  public static func - (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_attoseconds: lhs._attoseconds - rhs._attoseconds)
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
    return Duration(_attoseconds:
      _Int128(Double(lhs._attoseconds) / rhs))
  }

  public static func /= (_ lhs: inout Duration, _ rhs: Double) {
    lhs = lhs / rhs
  }

  public static func / <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(_attoseconds: lhs._attoseconds / _Int128(rhs))
  }

  public static func /= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs / rhs
  }

  public static func / (_ lhs: Duration, _ rhs: Duration) -> Double {
    Double(lhs._attoseconds) / Double(rhs._attoseconds)
  }

  public static func * (_ lhs: Duration, _ rhs: Double) -> Duration {
    Duration(_attoseconds: _Int128(Double(lhs._attoseconds) * rhs))
  }

  public static func * <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(_attoseconds: lhs._attoseconds * _Int128(rhs))
  }

  public static func *= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs * rhs
  }
}

extension Duration: CustomStringConvertible {
  public var description: String {
    return (Double(_attoseconds) / 1e18).description + " seconds"
  }
}

extension Duration: DurationProtocol { }
