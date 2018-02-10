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

/// A type that allows random number generators to be used throughout Swift to
/// produce random aspects of Swift's types.
///
/// The RandomNumberGenerator protocol is used for random number generator types
/// that implement their own pseudo-random or cryptographically secure
/// pseudo-random number generation. Using the RandomNumberGenerator protocol
/// allows these types to be used with types with random methods, collections
/// with .random(), and collections/sequences with
/// .shuffle() and .shuffled()
///
/// Conforming to the RandomNumberGenerator protocol
/// ==========================================
///
/// In order to conform to RandomNumberGenerator, types that implement this must be
/// able to produce an unsigned integer.
///
/// - Note: Types that implement RandomNumberGenerator should have a decent
///   amount of documentation that clearly conveys whether or not the generator
///   produces cryptographically secure numbers or deterministically generated
///   numbers.
public protocol RandomNumberGenerator {
  /// Produces the next randomly generated number
  ///
  /// - Returns: A number that was randomly generated
  func next() -> UInt64
}

extension RandomNumberGenerator {
  /// Produces the next randomly generated number
  ///
  /// - Returns: A number that was randomly generated
  ///
  /// This differs from next() as this function has the ability to transform the
  /// generated number to any unsigned integer.
  @_inlineable
  public func next<T: FixedWidthInteger & UnsignedInteger>() -> T {
    if T.bitWidth == UInt64.bitWidth {
      return T(self.next())
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

  /// Produces the next randomly generated number that is constricted by an
  /// upperBound
  ///
  /// - Parameter upperBound: The max number this can generate up to.
  /// - Returns: A number that was randomly generated from 0 to upperBound
  ///
  /// This uses the uniform distribution to form a random number within the
  /// upperBound.
  @_inlineable
  public func next<T: FixedWidthInteger & UnsignedInteger>(upperBound: T) -> T {
    let range = T.max % upperBound
    var random: T = 0

    repeat {
      random = self.next()
    } while random < range

    return random % upperBound
  }
}

/// The provided default source of random numbers
///
/// All of the default provided random functions utilize this source of random.
/// Using those functions should be preferred over using this directly. An
/// example of calling this directly:
///
///     let random: UInt8 = Random.default.next()
///     let randomToTen: UInt32 = Random.default.next(upperBound: 10)
///
/// However, you should strive to use the random functions on the numeric types.
/// Using the preferred way:
///
///     let random = UInt8.random(in: .min ... .max)
///     let randomToTen = UInt32.random(in: 0 ..< 10)
///
/// - Note: The default implementation of randomness is cryptographically secure.
///   It utilizes arc4random(3) on newer versions of macOS, iOS, etc. On older
///   versions of these operating systems it uses SecRandomCopyBytes. For Linux,
///   it tries to use the getrandom(2) system call on newer kernel versions. On
///   older kernel versions, it reads from /dev/urandom.
public struct Random : RandomNumberGenerator {
  /// The default random implementation
  public static let `default` = Random()

  private init() {}

  /// Produces the next randomly generated number
  ///
  /// - Returns: A number that was randomly generated
  public func next() -> UInt64 {
    var random: UInt64 = 0
    _stdlib_random(&random, MemoryLayout<UInt64>.size)
    return random
  }
  
  /// Produces the next randomly generated number
  ///
  /// - Returns: A number that was randomly generated
  ///
  /// This differs from next() as this function has the ability to transform the
  /// generated number to any unsigned integer.
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
