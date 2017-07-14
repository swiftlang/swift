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
import Foundation

@inline(never)
public func run_ReduceIntoInt(_ N: Int) {
  let numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*100 {
    c = c &+ numbers.reduce(into: 0) { (acc: inout Int, num: Int) in
      acc = acc &+ num
    }
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_ReduceIntoArray(_ N: Int) {
  let numbers = [Int](0..<100)
  
  var c = 0
  for _ in 1...N*100 {
    let a = numbers.reduce(into: []) { (acc: inout [Int], num: Int) in
      acc.append(num)
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_ReduceIntoDictionary(_ N: Int) {
  let numbers = [Int](0..<100)
  
  var c = 0
  for _ in 1...N*100 {
    let d = numbers.reduce(into: [:]) { (acc: inout [Int: Int], num: Int) in
      acc[num] = num
    }
    c = c &+ d.count
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_MapUsingReduceInto(_ N: Int) {
  let numbers = [Int](0..<100)
  
  var c = 0
  let f: (Int) -> Int = { $0 &+ 5 }
  for _ in 1...N*100 {
    let a = numbers.reduce(into: []) { (acc: inout [Int], x: Int) in
      acc.append(f(x))
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}

@inline(never)
public func run_FrequenciesUsingReduceInto(_ N: Int) {
  let s = "thequickbrownfoxjumpsoverthelazydogusingasmanycharacteraspossible123456789"
  
  var c = 0
  for _ in 1...N*100 {
    let a = s.reduce(into: [:]) { (acc: inout [Character: Int], c: Character) in
      acc[c, default: 0] += 1
    }
    c = c &+ a.count
  }
  CheckResults(c != 0)
}
