//===--- ReduceInto.swift -------------------------------------------------===//
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

import TestsUtils

public let ReduceInto = [
  BenchmarkInfo(name: "FilterEvenUsingReduce", runFunction: run_FilterEvenUsingReduce, tags: [.validation, .api], legacyFactor: 10),
  BenchmarkInfo(name: "FilterEvenUsingReduceInto", runFunction: run_FilterEvenUsingReduceInto, tags: [.validation, .api]),
  BenchmarkInfo(name: "FrequenciesUsingReduce", runFunction: run_FrequenciesUsingReduce, tags: [.validation, .api], legacyFactor: 10),
  BenchmarkInfo(name: "FrequenciesUsingReduceInto", runFunction: run_FrequenciesUsingReduceInto, tags: [.validation, .api], legacyFactor: 10),
  BenchmarkInfo(name: "SumUsingReduce", runFunction: run_SumUsingReduce, tags: [.validation, .api]),
  BenchmarkInfo(name: "SumUsingReduceInto", runFunction: run_SumUsingReduceInto, tags: [.validation, .api]),
]

// Sum

@inline(never)
public func run_SumUsingReduce(_ N: Int) {
  let numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*1000 {
    c = c &+ numbers.reduce(0) { (acc: Int, num: Int) -> Int in
      acc &+ num
    }
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_SumUsingReduceInto(_ N: Int) {
  let numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*1000 {
    c = c &+ numbers.reduce(into: 0) { (acc: inout Int, num: Int) in
      acc = acc &+ num
    }
  }
  CheckResults(c != 0)
}

// Filter

@inline(never)
public func run_FilterEvenUsingReduce(_ N: Int) {
  let numbers = [Int](0..<100)

  var c = 0
  for _ in 1...N*10 {
    let a = numbers.reduce([]) { (acc: [Int], num: Int) -> [Int] in
      var a = acc
      if num % 2 == 0 {
        a.append(num)
      }
      return a
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_FilterEvenUsingReduceInto(_ N: Int) {
  let numbers = [Int](0..<100)

  var c = 0
  for _ in 1...N*100 {
    let a = numbers.reduce(into: []) { (acc: inout [Int], num: Int) in
      if num % 2 == 0 {
        acc.append(num)
      }
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}

// Frequencies

@inline(never)
public func run_FrequenciesUsingReduce(_ N: Int) {
  let s = "thequickbrownfoxjumpsoverthelazydogusingasmanycharacteraspossible123456789"

  var c = 0
  for _ in 1...N*10 {
    let a = s.reduce([:]) {
      (acc: [Character: Int], c: Character) -> [Character: Int] in
      var d = acc
      d[c, default: 0] += 1
      return d
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_FrequenciesUsingReduceInto(_ N: Int) {
  let s = "thequickbrownfoxjumpsoverthelazydogusingasmanycharacteraspossible123456789"

  var c = 0
  for _ in 1...N*10 {
    let a = s.reduce(into: [:]) {
      (acc: inout [Character: Int], c: Character) in
      acc[c, default: 0] += 1
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}
