//===--- DictTest3.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

@inline(never)
public func run_Dictionary3(N: Int) {
  let size1 = 100
  let reps = 20
  let ref_result = "1 99 20 1980"
  var hash1 = [String:Int]()
  var hash2 = [String:Int]()
  var res = ""

  for _ in 1...N {
    hash1 = [:]
    for i in 0..<size1 {
      hash1["foo_" + String(i)] = i
    }

    hash2 = hash1

    for _ in 1..<reps {
      for (k, v) in hash1 {
        hash2[k] = hash2[k]! + v
      }
    }

    res = (String(hash1["foo_1"]!) + " " + String(hash1["foo_99"]!) + " " +
           String(hash2["foo_1"]!) + " " + String(hash2["foo_99"]!))
    if res != ref_result {
      break
    }
  }
  CheckResults(res == ref_result, "Incorrect results in Dictionary3: \(res) != \(ref_result)")
}
