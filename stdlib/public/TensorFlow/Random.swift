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
public final class UniformFloatingPointDistribution<T : BinaryFloatingPoint>
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
public final class NormalDistribution<T : BinaryFloatingPoint>
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

@_fixed_layout
public final class BetaDistribution {
  public let alpha: Float
  public let beta: Float
  private let uniformDistribution = UniformFloatingPointDistribution<Float>()

  public init(alpha: Float = 0, beta: Float = 1) {
    self.alpha = alpha
    self.beta = beta
  }

  public func next<G: RandomNumberGenerator>(using rng: inout G) -> Float {
    // Generate a sample using Cheng's sampling algorithm from:
    // R. C. H. Cheng, "Generating beta variates with nonintegral shape 
    // parameters.". Communications of the ACM, 21, 317-322, 1978.
    let a = min(alpha, beta)
    let b = max(alpha, beta)
    if a > 1 {
      return BetaDistribution.chengsAlgorithmBB(alpha, a, b, using: &rng)
    } else {
      return BetaDistribution.chengsAlgorithmBC(alpha, b, a, using: &rng)
    }
  }

  /// Returns one sample from a Beta(alpha, beta) distribution using Cheng's BB 
  /// algorithm, when both alpha and beta are greater than 1.
  ///
  /// - Parameters:
  ///   - alpha: First Beta distribution shape parameter.
  ///   - a: `min(alpha, beta)`.
  ///   - b: `max(alpha, beta)`.
  ///   - rng: Random number generator.
  ///
  /// - Returns: Sample obtained using Cheng's BB algorithm.
  private static func chengsAlgorithmBB<G: RandomNumberGenerator>(
    _ alpha0: Float, 
    _ a: Float, 
    _ b: Float, 
    using rng: inout G
  ) -> Float {
    let alpha = a + b
    let beta  = sqrt((alpha - 2) / (2 * a * b - alpha))
    let gamma = a + 1 / beta

    var r: Float = 0.0
    var w: Float = 0.0
    var t: Float = 0.0

    repeat {
      let u1 = Float.random(in: 0.0...1.0, using: &rng)
      let u2 = Float.random(in: 0.0...1.0, using: &rng)
      let v = beta * (log(u1) - log1p(-u1))
      r = gamma * v - 1.3862944
      let z = u1 * u1 * u2
      w = a * exp(v)

      let s = a + r - w
      if s + 2.609438 >= 5 * z {
        break
      }

      t = log(z)
      if s >= t {
        break
      }
    } while r + alpha * (log(alpha) - log(b + w)) < t

    w = min(w, Float.greatestFiniteMagnitude)
    return a == alpha0 ? w / (b + w) : b / (b + w)
  }

  /// Returns one sample from a Beta(alpha, beta) distribution using Cheng's BC 
  /// algorithm, when at least one of alpha and beta is less than 1.
  ///
  /// - Parameters:
  ///     - alpha: First Beta distribution shape parameter.
  ///     - a: `max(alpha, beta)`.
  ///     - b: `min(alpha, beta)`.
  ///     - rng: Random number generator.
  ///
  /// - Returns: Sample obtained using Cheng's BB algorithm.
  private static func chengsAlgorithmBC<G: RandomNumberGenerator>(
    _ alpha0: Float, 
    _ a: Float, 
    _ b: Float, 
    using rng: inout G
  ) -> Float {
    let alpha = a + b
    let beta  = 1 / b
    let delta = 1 + a - b
    let k1    = delta * (0.0138889 + 0.0416667 * b) / (a * beta - 0.777778)
    let k2    = 0.25 + (0.5 + 0.25 / delta) * b

    var w: Float = 0.0
    
    while true {
      let u1 = Float.random(in: 0.0...1.0, using: &rng)
      let u2 = Float.random(in: 0.0...1.0, using: &rng)
      let y = u1 * u2
      let z = u1 * y

      if u1 < 0.5 {
        if 0.25 * u2 + z - y >= k1 {
          continue
        }
      } else {
        if z <= 0.25 {
          let v = beta * (log(u1) - log1p(-u1))
          w = a * exp(v)
          break
        }
        if z >= k2 {
          continue
        }
      }

      let v = beta * (log(u1) - log1p(-u1))
      w = a * exp(v)
      if alpha * (log(alpha) - log(b + 1) + v) - 1.3862944 >= log(z) {
        break
      }
    }

    w = min(w, Float.greatestFiniteMagnitude)
    return a == alpha0 ? w / (b + w) : b / (b + w)
  }
}
