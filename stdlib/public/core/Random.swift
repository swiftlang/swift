//===--- Random.swift ------------------------------------------------------===//
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
/// The RandomGenerator protocol is used for random number generator types that
/// implement their own pseudo-random or cryptographically-secure pseudo-random
/// number generation. Using the RandomGenerator protocol allows these types
/// to be used with types that conform to `Randomizable` or collections who
/// conform to `RandomAccessCollection`.
///
/// Conforming to the RandomGenerator protocol
/// ==========================================
///
/// In order to conform to RandomGenerator, types that implement this must be
/// able to produce different types of integers that conform to
/// `FixedWidthInteger`. They must also be able to constrict that number down
/// to an upperBound.
///
/// - Note: Types that implement RandomGenerator should have a decent amount of
///   documentation that clearly conveys whether or not the generator produces
///   cryptographically secure numbers or deterministically generated numbers.
public protocol RandomGenerator {
  /// Produces the next randomly generated number
  ///
  /// - Parameter type: The `FixedWidthInteger` type to generate
  /// - Returns: A number that was randomly generated
  func next<T : FixedWidthInteger>(_ type: T.Type) -> T

  /// Produces the next randomly generated number that is constricted by an
  /// upperBound
  ///
  /// - Parameter type: The `FixedWidthInteger` type to generate
  /// - Parameter upperBound: The max number this can generate up to.
  /// - Returns: A number that was randomly generated
  func next<T : FixedWidthInteger>(_ type: T.Type, upperBound: T) -> T
}

/// The provided default source of random numbers
///
/// All of the default provided random functions utilize this source of random.
/// Using those functions should be preferred over using this directly. An
/// example of calling this directly:
///
///     let random = Random.default.next(Int.self)
///     let randomToTen = Random.default.next(Int.self, upperBound: 10)
///
/// However, you should strive to use the random functions on the numeric types.
/// Using the preferred way:
///
///     let random = Int.random
///     let randomToTen = (0 ... 10).random
///
/// - Note: The default implementation of randomness is cryptographically secure.
///   It utilizes arc4random on newer versions of macOS, iOS, etc. On older
///   versions of these operating systems it uses /dev/urandom and SecRandomCopyBytes
///   on iOS. For Linux, it tries to use the getrandom(2) system call on newer
///   kernel versions. On older kernel versions, it uses /dev/urandom.
public enum Random : RandomGenerator {
  /// The default random implementation
  case `default`

  /// Produces the next randomly generated number
  ///
  /// - Parameter type: The `FixedWidthInteger` type to generate
  /// - Returns: A number that was randomly generated
  @_inlineable
  public func next<T : FixedWidthInteger>(_ type: T.Type) -> T {
    var random: T = 0
    _swift_stdlib_random(&random, MemoryLayout<T>.size, _fatalErrorFlags())
    return random
  }

  /// Produces the next randomly generated number that is constricted by an
  /// upperBound
  ///
  /// - Parameter type: The `FixedWidthInteger` type to generate
  /// - Parameter upperBound: The max number this can generate up to.
  /// - Returns: A number that was randomly generated
  @_inlineable
  public func next<T : FixedWidthInteger>(_ type: T.Type, upperBound: T) -> T {
    let range = T.max % upperBound
    var random: T = 0

    repeat {
      _swift_stdlib_random(&random, MemoryLayout<T>.size, _fatalErrorFlags())
    } while random >= range

    return random % upperBound
  }
}

/// A type that allows other types to be generated randomly
///
/// The Randomizable protocol is to be used when types have the capability of
/// being randomized. A good example of this is `Int`.
///
///     let randomInt = Int.random
///
/// `Int` conforms to the Randomizable protocol because there is the ability to
/// produce a random `Int`. Note that not all types that are able to implement
/// this necessariliy should.
///
/// Conforming to the Randomizable protocol
/// =======================================
///
/// In order to conform to the Randomizable protocol, all one must implement is
/// the random(using:) function. As an example, below is a custom Date structure:
///
///     struct Date {
///         let year: Int
///         let month: Int
///         let day: Int
///     }
///
/// In order for `Date` to conform to `Randomizable`, it must declare conformance
/// and implement the random(using:) function.
///
///     extension Date : Randomizable {
///         static func random(using generator: RandomGenerator) -> Self {
///             let randomYear = (0 ... 3000).random(using: generator)
///             let randomMonth = (1 ... 12).random(using: generator)
///             let randomDay = (1 ... 31).random(using: generator)
///             // This could produce a date with day 31 on months without 31 days
///             // Ideally, you should check for this
///             return Date(year: randomYear, month: randomMonth, day: randomDay)
///         }
///     }
///
/// Note how the ranges use the random(using:) function that is defined for them.
/// Make sure to do this for custom types as it allows developers the ability
/// to use their own random number generators on custom types.
public protocol Randomizable {
  /// The random representation of this type.
  ///
  /// Shorthand for using the random(using:) function. This uses the default
  /// random implementation defined in the standard library.
  static var random: Self { get }

  /// Returns a random representation of this type.
  ///
  /// - Parameter generator: The random number generator to use when getting
  ///   random values
  /// - Returns: A random representation of the type conforming to `Randomizable`
  static func random(using generator: RandomGenerator) -> Self
}

extension Randomizable {
  /// The random representation of this type.
  ///
  /// Shorthand for using the random(using:) function. This uses the default
  /// random implementation defined in the standard library.
  @_inlineable
  public static var random: Self {
    return self.random(using: Random.default)
  }
}
