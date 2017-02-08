//===--- NopDeinit.swift --------------------------------------------------===//
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

// <rdar://problem/17838787>
import TestsUtils

class X<T : Comparable> {
  let deinitIters = 10000
  var elem: T
  init(_ x : T) {elem = x}
  deinit {
    for _ in 1...deinitIters {
      if (elem > elem) { }
    }
  }
}

public func run_NopDeinit(_ N: Int) {
  for _ in 1...N {
    var arr :[X<Int>] = []
    let size = 500
    for i in 1...size { arr.append(X(i)) }
    arr.removeAll()
    CheckResults(arr.count == 0,
                 "Incorrect results in NopDeinit: \(arr.count) != 0.")
  }
}
