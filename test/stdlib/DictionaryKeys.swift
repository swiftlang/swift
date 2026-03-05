//===--- DictionaryKeys.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var testSuite = TestSuite("DictionaryKeys")

let multipleElementDict = ["One": 1, "Two": 2, "Three": 3, "Four": 4, "Five": 5]
let multipleElementDictMixedAndMatched = ["Five": 2, "One": 1, "Four": 4, "Three": 5, "Two": 3]
let singleElementDict = ["One": 1]
let emptyDict = [String: Int]()

if #available(SwiftStdlib 6.3, *) {
  testSuite.test("Hashable") {
    let keys = [multipleElementDict.keys, singleElementDict.keys, emptyDict.keys]
    checkHashable(keys, equalityOracle: { keys[$0] == keys[$1] })
  }

  testSuite.test("OrderIndependentHashing") {
    checkHashable(expectedEqual: true, multipleElementDict.keys, multipleElementDictMixedAndMatched.keys)
  }
}

runAllTests()
