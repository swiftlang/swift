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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
@frozen
public struct Duration: Sendable {
  @frozen
  @usableFromInline
  struct Bits: Sendable, Codable, Hashable, Equatable {
    @inlinable
    static var nanosecondMask: UInt64 { 0x000000003FFFFFFF }
    
    @inlinable
    static var secondsSignFlags: UInt64 { 0x8000000000000000 }
    
    @inlinable
    static var nanosecondsSignFlags: UInt64 { 0x8000000000000000 }

    @usableFromInline
    var high: UInt64
    
    @usableFromInline
    var low: UInt64
    
    @usableFromInline
    init(high: UInt64, low: UInt64) {
      self.high = high
      self.low = low
    }
  }
  
  @usableFromInline
  var bits: Bits
  
  @inlinable
  public var seconds: Int64 {
    if _sign.0 {
      return -Int64(bits.high)
    } else {
      return Int64(bits.high)
    }
  }
  
  @inlinable
  public var nanoseconds: Int64 {
    if _sign.1 {
      return -Int64(bitPattern: bits.low & Bits.nanosecondMask)
    } else {
      return Int64(bitPattern: bits.low & Bits.nanosecondMask)
    }
  }
  
  @inlinable
  public var _sign: (Bool, Bool) {
    return ((bits.high & Bits.secondsSignFlags) != 0, (bits.low & Bits.nanosecondsSignFlags) != 0)
  }
  
  @inlinable
  public init(_seconds seconds: Int64, nanoseconds: Int64) {
    
    let additionalSeconds = nanoseconds / 1000000000
    let s = seconds + Int64(additionalSeconds)
    let ns = nanoseconds - (additionalSeconds * 1000000000)
    
    bits = Bits(high: s.magnitude, low: ns.magnitude & Bits.nanosecondMask)
    if seconds < 0  {
      bits.low |= Bits.secondsSignFlags
    }
    if nanoseconds < 0 {
      bits.low |= Bits.nanosecondsSignFlags
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration {
  @inlinable
  public static func seconds<T: BinaryInteger>(_ seconds: T) -> Duration {
    return Duration(_seconds: Int64(seconds), nanoseconds: 0)
  }
  
  @inlinable
  public static func seconds(_ seconds: Double) -> Duration {
    let s = Int64(seconds)
    let ns = Int64((seconds - Double(s)) * 1_000_000_000)
    return Duration(_seconds: s, nanoseconds: ns)
  }
  
  @inlinable
  public static func milliseconds<T: BinaryInteger>(_ milliseconds: T) -> Duration {
    let s = milliseconds / 1000
    let ns = (milliseconds - s * 1000) * 1000000
    return Duration(_seconds: Int64(s), nanoseconds: Int64(ns))
  }
  
  @inlinable
  public static func milliseconds(_ milliseconds: Double) -> Duration {
    let s = Int64(milliseconds / 1000.0)
    let ns = Int64((milliseconds - Double(s) * 1000) * 1000000)
    return Duration(_seconds: s, nanoseconds: ns)
  }
  
  @inlinable
  public static func microseconds<T: BinaryInteger>(_ microseconds: T) -> Duration {
    let s = microseconds / 1000000
    let ns = (microseconds - s * 1000000) * 1000
    return Duration(_seconds: Int64(s), nanoseconds: Int64(ns))
  }
  
  @inlinable
  public static func microseconds(_ microseconds: Double) -> Duration {
    let s = Int64(microseconds / 1000000.0)
    let ns = Int64((microseconds - Double(s) * 1000000) * 1000)
    return Duration(_seconds: s, nanoseconds: ns)
  }
  
  @inlinable
  public static func nanoseconds<T: BinaryInteger>(_ nanoseconds: T) -> Duration {
    let s = nanoseconds / 1000000000
    let ns = nanoseconds - s * 1000000000
    return Duration(_seconds: Int64(s), nanoseconds: Int64(ns))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: Codable { }

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: Hashable { }

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: Equatable {
  @inlinable
  public static func == (_ lhs: Duration, _ rhs: Duration) -> Bool {
    return lhs.bits == rhs.bits
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: Comparable {
  @inlinable
  public static func < (_ lhs: Duration, _ rhs: Duration) -> Bool {
    if lhs.seconds < rhs.seconds { return true }
    if lhs.seconds > rhs.seconds { return false }
    return lhs.nanoseconds < rhs.nanoseconds
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: AdditiveArithmetic {
  @inlinable
  public static var zero: Duration { Duration(_seconds: 0, nanoseconds: 0) }
   
  @inlinable
  public static func + (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_seconds: lhs.seconds + rhs.seconds, nanoseconds: lhs.nanoseconds + rhs.nanoseconds)
  }
  
  @inlinable
  public static func - (_ lhs: Duration, _ rhs: Duration) -> Duration {
    return Duration(_seconds: lhs.seconds - rhs.seconds, nanoseconds: lhs.nanoseconds - rhs.nanoseconds)
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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration {
  @inlinable
  public static func / (_ lhs: Duration, _ rhs: Double) -> Duration {
    return Duration.nanoseconds(Int64(Double(lhs.seconds * 1000000000 + lhs.nanoseconds) / rhs))
  }
  
  @inlinable
  public static func /= (_ lhs: inout Duration, _ rhs: Double) {
    lhs = lhs / rhs
  }
  
  @inlinable
  public static func / <T: BinaryInteger>(_ lhs: Duration, _ rhs: T) -> Duration {
    return Duration.nanoseconds(T(lhs.seconds * 1000000000 + lhs.nanoseconds) / rhs)
  }
  
  @inlinable
  public static func /= <T: BinaryInteger>(_ lhs: inout Duration, _ rhs: T) {
    lhs = lhs / rhs
  }
  
  @inlinable
  public static func / (_ lhs: Duration, _ rhs: Duration) -> Double {
    return (Double(lhs.seconds) * 1000000000.0 + Double(lhs.nanoseconds)) / (Double(rhs.seconds) * 1000000000.0 + Double(rhs.nanoseconds))
  }
  
  @inlinable
  public static func * (_ lhs: Duration, _ rhs: Double) -> Duration {
    return Duration(_seconds: Int64(Double(lhs.seconds) * rhs), nanoseconds: Int64(Double(lhs.nanoseconds) * rhs))
  }
  
  @inlinable
  public static func * <T: BinaryInteger>(_ lhs: Duration, _ rhs: T) -> Duration {
    return Duration(_seconds: Int64(T(lhs.seconds) * rhs), nanoseconds: Int64(T(lhs.nanoseconds) * rhs))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Duration: CustomStringConvertible {
  @inlinable
  public var description: String {
    return (Double(seconds) + Double(nanoseconds) / 1_000_000_000.0).description + " seconds"
  }
}
