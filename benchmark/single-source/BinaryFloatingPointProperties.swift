//===--- BinaryFloatingPointProperties.swift ------------------------------===//
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

import Foundation
import TestsUtils

public let BinaryFloatingPointPropertiesBinade = BenchmarkInfo(
  name: "BinaryFloatingPointPropertiesBinade",
  runFunction: run_BinaryFloatingPointPropertiesBinade,
  tags: [.validation, .algorithm]
)

public let BinaryFloatingPointPropertiesNextUp = BenchmarkInfo(
  name: "BinaryFloatingPointPropertiesNextUp",
  runFunction: run_BinaryFloatingPointPropertiesNextUp,
  tags: [.validation, .algorithm]
)

public let BinaryFloatingPointPropertiesUlp = BenchmarkInfo(
  name: "BinaryFloatingPointPropertiesUlp",
  runFunction: run_BinaryFloatingPointPropertiesUlp,
  tags: [.validation, .algorithm]
)

@inline(never)
public func run_BinaryFloatingPointPropertiesBinade(_ N: Int) {
  var xs = [Double]()
  xs.reserveCapacity(N)
  for _ in 1...N {
    var x = 0 as Double
    for i in 0..<10000 {
      x += Double(getInt(i)).binade
    }
    xs.append(x)
  }
  CheckResults(xs[getInt(0)] == 37180757)
}

@inline(never)
public func run_BinaryFloatingPointPropertiesNextUp(_ N: Int) {
  var xs = [Int]()
  xs.reserveCapacity(N)
  for _ in 1...N {
    var x = 0 as Int
    for i in 0..<10000 {
      x += Int(Double(getInt(i)).nextUp)
    }
    xs.append(x)
  }
  CheckResults(xs[getInt(0)] == 49995000)
}

@inline(never)
public func run_BinaryFloatingPointPropertiesUlp(_ N: Int) {
  var xs = [Int]()
  xs.reserveCapacity(N)
  for _ in 1...N {
    var x = 0 as Int
    for i in 0..<10000 {
      x += Int(Double(getInt(i)).ulp)
    }
    xs.append(x)
  }
  CheckResults(xs[getInt(0)] == 0)
}
