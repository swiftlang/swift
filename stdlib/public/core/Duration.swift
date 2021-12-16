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
  @usableFromInline
  var attoseconds: _DoubleWidth<Int64>
  
  @inlinable
  init(attoseconds: _DoubleWidth<Int64>) {
    self.attoseconds = attoseconds
  }
  
  init(seconds: Int64, nanoseconds: Int64) {
    self = Duration.seconds(seconds) + Duration.nanoseconds(nanoseconds)
  }
  
  @inlinable
  public var seconds: Int64 {
    Int64(attoseconds / 1_000_000_000_000_000_000)
  }
  
  @inlinable
  public var nanoseconds: Int64 {
    let seconds = attoseconds / 1_000_000_000_000_000_000
    let nanoseconds = 
      Int64((attoseconds - seconds * 1_000_000_000_000_000_000) / 1_000_000_000)
    return nanoseconds
  }
}

extension Duration {
  @inlinable
  public static func seconds<T: BinaryInteger>(_ seconds: T) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(seconds) * 
                                 1_000_000_000_000_000_000)
  }
  
  @inlinable
  public static func seconds(_ seconds: Double) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(seconds * 
                                 1_000_000_000_000_000_000))
  }
  
  @inlinable
  public static func milliseconds<T: BinaryInteger>(
    _ milliseconds: T
  ) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(milliseconds) * 
                                 1_000_000_000_000_000)
  }
  
  @inlinable
  public static func milliseconds(_ milliseconds: Double) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(milliseconds * 
                                 1_000_000_000_000_000))
  }
  
  @inlinable
  public static func microseconds<T: BinaryInteger>(
    _ microseconds: T
  ) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(microseconds) * 
                                 1_000_000_000_000)
  }
  
  @inlinable
  public static func microseconds(_ microseconds: Double) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(microseconds * 
                                 1_000_000_000_000))
  }
  
  @inlinable
  public static func nanoseconds<T: BinaryInteger>(
    _ nanoseconds: T
  ) -> Duration {
    return Duration(attoseconds: _DoubleWidth<Int64>(nanoseconds) * 
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
    self.init(seconds: seconds, nanoseconds: nanoseconds)
  }
  
  public func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    try container.encode(seconds, forKey: .seconds)
    try container.encode(nanoseconds, forKey: .nanoseconds)
  }
}

extension Duration: Hashable { }

extension Duration: Equatable {
  @inlinable
  public static func == (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs.attoseconds == rhs.attoseconds
  }
}

extension Duration: Comparable {
  @inlinable
  public static func < (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs.attoseconds < rhs.attoseconds
  }
}

extension Duration: AdditiveArithmetic {
  @inlinable
  public static var zero: Duration { Duration(attoseconds: 0) }
   
  @inlinable
  public static func + (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(attoseconds: lhs.attoseconds + rhs.attoseconds)
  }
  
  @inlinable
  public static func - (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(attoseconds: lhs.attoseconds - rhs.attoseconds)
  }
  
  @inlinable
  public static func += (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs + rhs
  }
  
  @inlinable
  public static func -= (_ lhs: inout Duration, _ rhs: Duration) {
    lhs = lhs - rhs
  }
}

extension Duration {
  @inlinable
  public static func / (_ lhs: Duration, _ rhs: Double) -> Duration {
    return Duration(attoseconds: 
      _DoubleWidth<Int64>(Double(lhs.attoseconds) / rhs))
  }
  
  @inlinable
  public static func /= (_ lhs: inout Duration, _ rhs: Double) {
    lhs = lhs / rhs
  }
  
  @inlinable
  public static func / <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(attoseconds: lhs.attoseconds / _DoubleWidth<Int64>(rhs))
  }
  
  @inlinable
  public static func /= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs / rhs
  }
  
  @inlinable
  public static func / (_ lhs: Duration, _ rhs: Duration) -> Double {
    Double(lhs.attoseconds) / Double(rhs.attoseconds)
  }
  
  @inlinable
  public static func * (_ lhs: Duration, _ rhs: Double) -> Duration {
    Duration(attoseconds: _DoubleWidth<Int64>(Double(lhs.attoseconds) * rhs))
  }
  
  @inlinable
  public static func * <T: BinaryInteger>(
    _ lhs: Duration, _ rhs: T
  ) -> Duration {
    Duration(attoseconds: lhs.attoseconds * _DoubleWidth<Int64>(rhs))
  }
  
  @inlinable
  public static func *= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs * rhs
  }
}

extension Duration: CustomStringConvertible {
  @inlinable
  public var description: String {
    return (Double(attoseconds) / 1e18).description + " seconds"
  }
}

extension Duration: DurationProtocol { }
