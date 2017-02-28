//===--- MapReduce.swift --------------------------------------------------===//
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
public func run_MapReduce(_ N: Int) {
  var numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*100 {
    numbers = numbers.map { $0 &+ 5 }
    c += numbers.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceAnyCollection(_ N: Int) {
  let numbers = AnyCollection([Int](0..<1000))

  var c = 0
  for _ in 1...N*100 {
    let mapped = numbers.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceAnyCollectionShort(_ N: Int) {
  let numbers = AnyCollection([Int](0..<10))

  var c = 0
  for _ in 1...N*10000 {
    let mapped = numbers.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceShort(_ N: Int) {
  var numbers = [Int](0..<10)

  var c = 0
  for _ in 1...N*10000 {
    numbers = numbers.map { $0 &+ 5 }
    c += numbers.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceSequence(_ N: Int) {
  let numbers = sequence(first: 0) { $0 < 1000 ? $0 &+ 1 : nil }

  var c = 0
  for _ in 1...N*100 {
    let mapped = numbers.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceLazySequence(_ N: Int) {
  let numbers = sequence(first: 0) { $0 < 1000 ? $0 &+ 1 : nil }

  var c = 0
  for _ in 1...N*100 {
    let mapped = numbers.lazy.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceLazyCollection(_ N: Int) {
  let numbers = [Int](0..<1000)

  var c = 0
  for _ in 1...N*100 {
    let mapped = numbers.lazy.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceLazyCollectionShort(_ N: Int) {
  let numbers = [Int](0..<10)

  var c = 0
  for _ in 1...N*10000 {
    let mapped = numbers.lazy.map { $0 &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceString(_ N: Int) {
  let s = "thequickbrownfoxjumpsoverthelazydogusingasmanycharacteraspossible123456789"

  var c: UInt64 = 0
  for _ in 1...N*100 {
    c += s.utf8.map { UInt64($0 &+ 5) }.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceShortString(_ N: Int) {
  let s = "12345"

  var c: UInt64 = 0
  for _ in 1...N*100 {
    c += s.utf8.map { UInt64($0 &+ 5) }.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceClass(_ N: Int) {
  let numbers = (0..<1000).map { NSDecimalNumber(integerLiteral: $0) }

  var c = 0
  for _ in 1...N*100 {
    let mapped = numbers.map { $0.intValue &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

@inline(never)
public func run_MapReduceClassShort(_ N: Int) {
  let numbers = (0..<10).map { NSDecimalNumber(integerLiteral: $0) }

  var c = 0
  for _ in 1...N*10000 {
    let mapped = numbers.map { $0.intValue &+ 5 }
    c += mapped.reduce(0, &+)
  }
  CheckResults(c != 0, "IncorrectResults in MapReduce")
}

