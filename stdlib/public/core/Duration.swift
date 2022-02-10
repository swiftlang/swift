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

@available(SwiftStdlib 5.7, *)
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

  public init(seconds: Int64, attoseconds: Int64) {
    self = Duration.seconds(seconds) + 
           Duration(_attoseconds: _Int128(attoseconds))
  }

  internal var _attoseconds: _Int128 {
    _Int128(high: _high, low: _low)
  }

  public var components: (seconds: Int64, attoseconds: Int64) {
    let seconds = _attoseconds / 1_000_000_000_000_000_000
    let attoseconds =
      Int64((_attoseconds - seconds * 1_000_000_000_000_000_000))
    return (Int64(seconds), attoseconds)
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration {
  @available(SwiftStdlib 5.7, *)
  public static func seconds<T: BinaryInteger>(_ seconds: T) -> Duration {
    return Duration(_attoseconds: _Int128(seconds) *
                                 1_000_000_000_000_000_000)
  }

  @available(SwiftStdlib 5.7, *)
  public static func seconds(_ seconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(seconds *
                                 1_000_000_000_000_000_000))
  }

  @available(SwiftStdlib 5.7, *)
  public static func milliseconds<T: BinaryInteger>(
    _ milliseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(milliseconds) *
                                 1_000_000_000_000_000)
  }

  @available(SwiftStdlib 5.7, *)
  public static func milliseconds(_ milliseconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(milliseconds *
                                 1_000_000_000_000_000))
  }

  @available(SwiftStdlib 5.7, *)
  public static func microseconds<T: BinaryInteger>(
    _ microseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(microseconds) *
                                 1_000_000_000_000)
  }

  @available(SwiftStdlib 5.7, *)
  public static func microseconds(_ microseconds: Double) -> Duration {
    return Duration(_attoseconds: _Int128(microseconds *
                                 1_000_000_000_000))
  }

  @available(SwiftStdlib 5.7, *)
  public static func nanoseconds<T: BinaryInteger>(
    _ nanoseconds: T
  ) -> Duration {
    return Duration(_attoseconds: _Int128(nanoseconds) *
                                 1_000_000_000)
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: Codable { 
  @available(SwiftStdlib 5.7, *)
  public init(from decoder: Decoder) throws {
    let container = try decoder.unkeyedContainer()
    let high = container.decode(Int64.self)
    let low = container.decode(UInt64.self)
    self.init(_attoseconds: _Int128(high: high, low: low))
  }
  
  @available(SwiftStdlib 5.7, *)
  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(_high)
    try container.encode(_low)
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: Hashable { 
  @available(SwiftStdlib 5.7, *)
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_attoseconds)
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: Equatable {
  @available(SwiftStdlib 5.7, *)
  public static func == (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs._attoseconds == rhs._attoseconds
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: Comparable {
  @available(SwiftStdlib 5.7, *)
  public static func < (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs._attoseconds < rhs._attoseconds
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: AdditiveArithmetic {
  @available(SwiftStdlib 5.7, *)
  public static var zero: Duration { Duration(_attoseconds: 0) }

  @available(SwiftStdlib 5.7, *)
  public static func + (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_attoseconds: lhs._attoseconds + rhs._attoseconds)
  }

  @available(SwiftStdlib 5.7, *)
  public static func - (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_attoseconds: lhs._attoseconds - rhs._attoseconds)
  }

  @available(SwiftStdlib 5.7, *)
  public static func += (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs + rhs
  }

  @available(SwiftStdlib 5.7, *)
  public static func -= (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs - rhs
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration {
  @available(SwiftStdlib 5.7, *)
  public static func / (_ lhs: Duration, _ rhs: Double) -> Duration {
    return Duration(_attoseconds:
      _Int128(Double(lhs._attoseconds) / rhs))
  }

  @available(SwiftStdlib 5.7, *)
  public static func /= (_ lhs: inout Duration, _ rhs: Double) {
    lhs = lhs / rhs
  }

  @available(SwiftStdlib 5.7, *)
  public static func / <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(_attoseconds: lhs._attoseconds / _Int128(rhs))
  }

  @available(SwiftStdlib 5.7, *)
  public static func /= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs / rhs
  }

  @available(SwiftStdlib 5.7, *)
  public static func / (_ lhs: Duration, _ rhs: Duration) -> Double {
    Double(lhs._attoseconds) / Double(rhs._attoseconds)
  }

  @available(SwiftStdlib 5.7, *)
  public static func * (_ lhs: Duration, _ rhs: Double) -> Duration {
    Duration(_attoseconds: _Int128(Double(lhs._attoseconds) * rhs))
  }

  @available(SwiftStdlib 5.7, *)
  public static func * <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(_attoseconds: lhs._attoseconds * _Int128(rhs))
  }

  @available(SwiftStdlib 5.7, *)
  public static func *= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs * rhs
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: CustomStringConvertible {
  @available(SwiftStdlib 5.7, *)
  public var description: String {
    return (Double(_attoseconds) / 1e18).description + " seconds"
  }
}

@available(SwiftStdlib 5.7, *)
extension Duration: DurationProtocol { }
