//===--- DictionaryBridgeToObjC.swift -------------------------------------===//
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

// Performance benchmark for common operations on Dictionary values bridged to
// NSDictionary.
import TestsUtils
#if _runtime(_ObjC)
import Foundation

public let DictionaryBridgeToObjC = [
  BenchmarkInfo(
    name: "DictionaryBridgeToObjC_Bridge",
    runFunction: run_DictionaryBridgeToObjC_BridgeToObjC,
    tags: [.validation, .api, .Dictionary, .bridging]),
  BenchmarkInfo(
    name: "DictionaryBridgeToObjC_Access",
    runFunction: run_DictionaryBridgeToObjC_Access,
    tags: [.validation, .api, .Dictionary, .bridging]),
  BenchmarkInfo(
    name: "DictionaryBridgeToObjC_BulkAccess",
    runFunction: run_DictionaryBridgeToObjC_BulkAccess,
    tags: [.validation, .api, .Dictionary, .bridging])
]

let numbers: [String: Int] = [
  "one": 1,
  "two": 2,
  "three": 3,
  "four": 4,
  "five": 5,
  "six": 6,
  "seven": 7,
  "eight": 8,
  "nine": 9,
  "ten": 10,
  "eleven": 11,
  "twelve": 12,
  "thirteen": 13,
  "fourteen": 14,
  "fifteen": 15,
  "sixteen": 16,
  "seventeen": 17,
  "eighteen": 18,
  "nineteen": 19,
  "twenty": 20
]

@inline(never)
public func run_DictionaryBridgeToObjC_BridgeToObjC(_ N: Int) {
  for _ in 1 ... 100 * N {
    blackHole(numbers as NSDictionary)
  }
}

@inline(never)
public func run_DictionaryBridgeToObjC_Access(_ N: Int) {
  let d = numbers as NSDictionary
  blackHole(d.object(forKey: "one")) // Force bridging of contents
  for _ in 1 ... 100 * N {
    for key in numbers.keys {
      CheckResults(identity(d).object(forKey: key) != nil)
    }
  }
}

@inline(never)
public func run_DictionaryBridgeToObjC_BulkAccess(_ N: Int) {
  let d = numbers as NSDictionary
  for _ in 1 ... 100 * N {
    let d2 = NSDictionary(dictionary: identity(d))
    CheckResults(d2.count == d.count)
  }
}

#else // !_runtime(ObjC)
public let DictionaryBridgeToObjC: [BenchmarkInfo] = []
#endif
