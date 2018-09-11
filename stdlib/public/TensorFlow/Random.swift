//===-- Random.swift ------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A (pseudo-)random number generation library. The library separates concerns
// into engines which generate random bytes and distributions which use an
// engine to generate values from some statistical distribution.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif

/// A type that provides seedable deterministic pseudo-random data.
///
/// A SeedableRandomNumberGenerator can be used anywhere where a
/// RandomNumberGenerator would be used. It is useful when the pseudo-random
/// data needs to be reproducible across runs.
///
/// Conforming to the SeedableRandomNumberGenerator Protocol
/// ========================================================
///
/// To make a custom type conform to the `SeedableRandomNumberGenerator`
/// protocol, implement the `init(seed: [UInt8])` initializer, as well as the
/// requirements for `RandomNumberGenerator`. The values returned by `next()`
/// must form a deterministic sequence that depends only on the seed provided
/// upon initialization.
public protocol SeedableRandomNumberGenerator: RandomNumberGenerator {
  init(seed: [UInt8])
  init<T: BinaryInteger>(seed: T)
}

extension SeedableRandomNumberGenerator {
  public init<T: BinaryInteger>(seed: T) {
    var newSeed: [UInt8] = []
    for i in 0..<seed.bitWidth / UInt8.bitWidth {
      newSeed.append(UInt8(truncatingIfNeeded: seed >> (UInt8.bitWidth * i)))
    }
    self.init(seed: newSeed)
  }
}

/// An implementation of `SeedableRandomNumberGenerator` using ARC4.
///
/// ARC4 is a stream cipher that generates a pseudo-random stream of bytes. This
/// PRNG uses the seed as its key.
///
/// ARC4 is described in Schneier, B., "Applied Cryptography: Protocols,
/// Algorithms, and Source Code in C", 2nd Edition, 1996.
///
/// An individual generator is not thread-safe, but distinct generators do not
/// share state. The random data generated is of high-quality, but is not
/// suitable for cryptographic applications.
@_fixed_layout
public struct ARC4RandomNumberGenerator: SeedableRandomNumberGenerator {
  public static var global = ARC4RandomNumberGenerator(seed: UInt32(time(nil)))
  var state: [UInt8] = Array(0...255)
  var iPos: UInt8 = 0
  var jPos: UInt8 = 0

  /// Initialize ARC4RandomNumberGenerator using an array of UInt8. The array
  /// must have length between 1 and 256 inclusive.
  public init(seed: [UInt8]) {
    precondition(seed.count > 0, "Length of seed must be positive")
    precondition(seed.count <= 256, "Length of seed must be at most 256")
    var j: UInt8 = 0
    for i: UInt8 in 0...255 {
      j &+= S(i) &+ seed[Int(i) % seed.count]
      swapAt(i, j)
    }
  }

  // Produce the next random UInt64 from the stream, and advance the internal
  // state.
  public mutating func next() -> UInt64 {
    var result: UInt64 = 0
    for _ in 0..<UInt64.bitWidth / UInt8.bitWidth {
      result <<= UInt8.bitWidth
      result += UInt64(nextByte())
    }
    return result
  }

  // Helper to access the state.
  private func S(_ index: UInt8) -> UInt8 {
    return state[Int(index)]
  }

  // Helper to swap elements of the state.
  private mutating func swapAt(_ i: UInt8, _ j: UInt8) {
    state.swapAt(Int(i), Int(j))
  }

  // Generates the next byte in the keystream.
  private mutating func nextByte() -> UInt8 {
    iPos &+= 1
    jPos &+= S(iPos)
    swapAt(iPos, jPos)
    return S(S(iPos) &+ S(jPos))
  }
}

//===----------------------------------------------------------------------===//
// Distributions
//===----------------------------------------------------------------------===//

@_fixed_layout
public final class UniformIntegerDistribution<T: FixedWidthInteger> {
  public let lowerBound: T
  public let upperBound: T

  public init(lowerBound: T = T.self.min, upperBound: T = T.self.max) {
    self.lowerBound = lowerBound
    self.upperBound = upperBound
  }

  public func next<G: RandomNumberGenerator>(using rng: inout G) -> T {
    return T.random(in: lowerBound...upperBound, using: &rng)
  }
}

@_fixed_layout
public final class UniformFloatingPointDistribution<T: BinaryFloatingPoint>
  where T.RawSignificand : FixedWidthInteger {
  public let lowerBound: T
  public let upperBound: T

  public init(lowerBound: T = 0, upperBound: T = 1) {
    self.lowerBound = lowerBound
    self.upperBound = upperBound
  }

  public func next<G: RandomNumberGenerator>(using rng: inout G) -> T {
    return T.random(in: lowerBound..<upperBound, using: &rng)
  }
}

@_fixed_layout
public final class NormalDistribution<T: BinaryFloatingPoint>
  where T.RawSignificand : FixedWidthInteger {
  public let mean: T
  public let standardDeviation: T
  private let uniformDist = UniformFloatingPointDistribution<T>()

  public init(mean: T = 0, standardDeviation: T = 1) {
    self.mean = mean
    self.standardDeviation = standardDeviation
  }

  public func next<G: RandomNumberGenerator>(using rng: inout G) -> T {
    // FIXME: Box-Muller can generate two values for only a little more than the
    // cost of one.
    let u1 = uniformDist.next(using: &rng)
    let u2 = uniformDist.next(using: &rng)
    let r = (-2 * T(log(Double(u1)))).squareRoot()
    let theta: Double = 2 * Double.pi * Double(u2)
    let normal01 = r * T(cos(theta))
    return mean + standardDeviation * normal01
  }
}
