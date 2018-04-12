//===--- Random.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// A type that can provide uniformly distributed random data.
///
/// When you call methods that use random data, such as creating new random
/// values or shuffling a collection, you can pass a `RandomNumberGenerator`
/// type to be used as the source for randomness. When you don't pass a
/// generator, the default `Random` type is used.
///
/// When providing new APIs that use randomness, provide a version that accepts
/// a generator conforming to the `RandomNumberGenerator` protocol as well as a
/// version that uses the default generator. For example, this `Weekday`
/// enumeration provides static methods that return a random day of the week:
///
///     enum Weekday : CaseIterable {
///         case sunday, monday, tuesday, wednesday, thursday, friday, saturday
///
///         static func randomWeekday<G: RandomNumberGenerator>(using generator: G) -> Weekday {
///             return Weekday.allCases.random(using: generator)!
///         }
///
///         static func randomWeekday() -> Weekday {
///             return Weekday.randomWeekday(using: Random.default)
///         }
///     }
///
/// Conforming to the RandomNumberGenerator Protocol
/// ================================================
///
/// A custom `RandomNumberGenerator` type can have different characteristics
/// than the default `Random` type. For example, a seedable generator can be
/// used to generate the same sequence of random values for testing purposes.
///
/// To make a custom type conform to the `RandomNumberGenerator` protocol,
/// implement the required `next()` method. Each call to `next()` must produce
/// a uniform and independent random value.
///
/// Types that conform to `RandomNumberGenerator` should specifically document
/// the thread safety and quality of the generator.
public protocol RandomNumberGenerator {
  /// Returns a value from a uniform, independent distribution of binary data.
  ///
  /// - Returns: An unsigned 64-bit random value.
  func next() -> UInt64
}

extension RandomNumberGenerator {
  /// Returns a value from a uniform, independent distribution of binary data.
  ///
  /// - Returns: A random value of `T`. Bits are randomly distributed so that
  ///   every value of `T` is equally likely to be returned.
  @inlinable
  public func next<T: FixedWidthInteger & UnsignedInteger>() -> T {
    if T.bitWidth <= UInt64.bitWidth {
      return T(truncatingIfNeeded: self.next())
    }

    let (quotient, remainder) = T.bitWidth.quotientAndRemainder(
      dividingBy: UInt64.bitWidth
    )
    var tmp: T = 0

    for i in 0 ..< quotient {
      tmp += T(truncatingIfNeeded: self.next()) &<< (UInt64.bitWidth * i)
    }

    if remainder != 0 {
      let random = self.next()
      let mask = UInt64.max &>> (UInt64.bitWidth - remainder)
      tmp += T(truncatingIfNeeded: random & mask) &<< (UInt64.bitWidth * quotient)
    }

    return tmp
  }

  /// Returns a random value that is less than the given upper bound.
  ///
  /// - Parameter upperBound: The upper bound for the randomly generated value.
  /// - Returns: A random value of `T` in the range `0..<upperBound`. Every
  ///   value in the range `0..<upperBound` is equally likely to be returned.
  @inlinable
  public func next<T: FixedWidthInteger & UnsignedInteger>(upperBound: T) -> T {
    let tmp = (T.max % upperBound) + 1
    let range = tmp == upperBound ? 0 : tmp
    var random: T = 0

    repeat {
      random = self.next()
    } while random < range

    return random % upperBound
  }
}

/// The default source of random data.
///
/// When you generate random values, shuffle a collection, or perform another
/// operation that depends on random data, this type's `default` property is
/// the generator used by default. For example, the two method calls in this
/// example are equivalent:
///
///     let x = Int.random(in: 1...100)
///     let y = Int.random(in: 1...100, using: Random.default)
///
/// `Random.default` is safe to use in multiple threads, and uses a
/// cryptographically secure algorithm whenever possible.
public struct Random : RandomNumberGenerator {
  /// The shared, default instance of the `Range` random number generator.
  public static let `default` = Random()

  private init() {}

  /// Returns a value from a uniform, independent distribution of binary data.
  ///
  /// - Returns: An unsigned 64-bit random value.
  public func next() -> UInt64 {
    var random: UInt64 = 0
    _stdlib_random(&random, MemoryLayout<UInt64>.size)
    return random
  }
  
  /// Returns a value from a uniform, independent distribution of binary data.
  ///
  /// - Returns: A random value of `T`. Bits are randomly distributed so that
  ///   every value of `T` is equally likely to be returned.
  public func next<T: FixedWidthInteger & UnsignedInteger>() -> T {
    var random: T = 0
    _stdlib_random(&random, MemoryLayout<T>.size)
    return random
  }
}

public // @testable
func _stdlib_random(_ bytes: UnsafeMutableRawBufferPointer) {
  if !bytes.isEmpty {
    _stdlib_random(bytes.baseAddress!, bytes.count)
  }
}
