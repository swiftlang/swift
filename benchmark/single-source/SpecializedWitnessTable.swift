//===--- SpecializedWitnessTable.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//  Tests calls through existentials of specialized generic types, e.g. an
//  `any P` which is created from a `Box<Int>`. With a specialized witness
//  table, such calls end up in witness methods which are specialized for the
//  concrete type instead of in the generic witness methods.
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "SpecializedWitnessTable.sum",
    runFunction: run_SpecializedWitnessTableSum,
    tags: [.validation, .abstraction, .existential],
    setUpFunction: { blackHole(numbers) }),
  BenchmarkInfo(
    name: "SpecializedWitnessTable.call",
    runFunction: run_SpecializedWitnessTableCall,
    tags: [.validation, .abstraction, .existential],
    setUpFunction: { blackHole(values) }),
]

protocol Summable {
  func sum() -> Int
}

struct Numbers<T: FixedWidthInteger>: Summable {
  var elements: [T]

  func sum() -> Int {
    var s: T = 0
    for e in elements {
      s &+= e
    }
    return Int(truncatingIfNeeded: s)
  }
}

protocol Valued {
  func value() -> Int
}

struct Wrapped<T: FixedWidthInteger>: Valued {
  var t: T

  func value() -> Int { Int(truncatingIfNeeded: t) }
}

@inline(never)
func makeNumbers() -> [any Summable] {
  return (0..<10).map { i in Numbers(elements: Array(0..<1_000).map { $0 + i }) }
}

@inline(never)
func makeValues() -> [any Valued] {
  return (0..<1_000).map { Wrapped(t: $0) }
}

let numbers = makeNumbers()
let values = makeValues()

@inline(never)
public func run_SpecializedWitnessTableSum(_ n: Int) {
  var c = 0
  for _ in 0..<10*n {
    for s in numbers {
      c &+= s.sum()
    }
  }
  check(c == 10*n * 5_040_000)
}

@inline(never)
public func run_SpecializedWitnessTableCall(_ n: Int) {
  var c = 0
  for _ in 0..<100*n {
    for v in values {
      c &+= v.value()
    }
  }
  check(c == 100*n * 499_500)
}
