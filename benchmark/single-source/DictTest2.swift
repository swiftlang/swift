//===--- DictTest2.swift --------------------------------------------------===//
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

@inline(never)
public func run_Dictionary2(_ N: Int) {
  let size = 500
  let ref_result = 199
  var res = 0
  for _ in 1...5*N {
    var x: [String: Int] = [:]
    for i in 1...size {
      x[String(i, radix:16)] = i
    }

    res = 0
    for i in 0..<size {
      let i2 = size-i
      if x[String(i2)] != nil {
        res += 1
      }
    }
    if res != ref_result {
      break
    }
  }
  CheckResults(res == ref_result, "Incorrect results in Dictionary2: \(res) != \(ref_result)")
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  var hashValue: Int {
    return value.hashValue
  }

  static func ==<T>(lhs: Box<T>, rhs: Box<T>) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_Dictionary2OfObjects(_ N: Int) {
  let size = 500
  let ref_result = 199
  var res = 0
  for _ in 1...5*N {
    var x: [Box<String>:Box<Int>] = [:]
    for i in 1...size {
      x[Box(String(i, radix:16))] = Box(i)
    }

    res = 0
    for i in 0..<size {
      let i2 = size-i
      if x[Box(String(i2))] != nil {
        res += 1
      }
    }
    if res != ref_result {
      break
    }
  }
  CheckResults(res == ref_result, "Incorrect results in Dictionary2AllObjects: \(res) != \(ref_result)")
}
