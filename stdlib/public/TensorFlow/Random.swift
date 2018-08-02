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

//===----------------------------------------------------------------------===//
// Engines
//===----------------------------------------------------------------------===//

public protocol RandomEngine: class {
  init(seed: [UInt8])
  init<T>(seed: T) where T : BinaryInteger
  func generate(count: Int) -> [UInt8]
}

@_fixed_layout
public class ARC4RandomEngine: RandomEngine {
  public static let global = ARC4RandomEngine(seed: UInt32(time(nil)))
  var state: [UInt8] = Array(0 ... 255)
  var iPos: UInt8 = 0
  var jPos: UInt8 = 0

  public required init(seed: [UInt8]) {
    var j: UInt8 = 0
    for i in 0 ..< 256 {
      j &+= state[Int(i)] &+ seed[Int(i) % seed.count]
      state.swapAt(Int(i), Int(j))
    }
  }

  public convenience required init<T>(seed: T) where T : BinaryInteger {
    var newSeed: [UInt8] = []
    for i in 0 ..< seed.bitWidth / UInt8.bitWidth {
      newSeed.append(UInt8(truncatingIfNeeded: seed >> (UInt8.bitWidth * i)))
    }
    self.init(seed: newSeed)
  }

  public required init(_ other: ARC4RandomEngine) {
    state = other.state
    iPos = other.iPos
    jPos = other.jPos
  }

  public func generate(count: Int) -> [UInt8] {
    var result: [UInt8] = []
    for _ in 0 ..< count {
      iPos &+= 1
      jPos &+= state[Int(iPos)]
      state.swapAt(Int(iPos), Int(jPos))
      result.append(state[Int(state[Int(iPos)] &+ state[Int(jPos)])])
    }
    return result
  }
}

//===----------------------------------------------------------------------===//
// Distributions
//===----------------------------------------------------------------------===//

public class UniformIntegerDistribution<T: BinaryInteger> {

  public init() { }

  public func generate(using engine: RandomEngine) -> T {
    var result: T = 0
    let bytes = engine.generate(count: result.bitWidth / 8)
    for b in bytes {
      result <<= 8
      result += T(b)
    }
    return result
  }
}

@_fixed_layout
public class UniformFloatingPointDistribution<T: BinaryFloatingPoint> {

  public let a: T
  public let b: T
  private let uniformIntDist = UniformIntegerDistribution<UInt64>()

  public init(a: T = 0, b: T = 1) {
    self.a = a
    self.b = b
  }

  public func generate(using engine: RandomEngine) -> T {
    let result = uniformIntDist.generate(using: engine)
    let uniform01: T = T(result) / (T(UInt64.max) + 1)
    return a + (b - a) * uniform01
  }
}

@_fixed_layout
public class NormalFloatingPointDistribution<T: BinaryFloatingPoint> {

  public let mean: T
  public let standardDeviation: T
  private let uniformDist = UniformFloatingPointDistribution<T>()
  private var cache: T? = nil

  public init(mean: T = 0, standardDeviation: T = 1) {
    self.mean = mean
    self.standardDeviation = standardDeviation
  }

  public func generate(using engine: RandomEngine) -> T {
    if let result = cache {
      cache = nil
      return result
    }
    let u1 = uniformDist.generate(using: engine)
    let u2 = uniformDist.generate(using: engine)
    let r = (-2 * T(log(Double(u1)))).squareRoot()
    let theta: Double = 2 * Double.pi * Double(u2)
    let normal0 = r * T(cos(theta))
    let normal1 = r * T(sin(theta))
    cache = mean + standardDeviation * normal1
    return mean + standardDeviation * normal0
  }
}
