//===--- Walsh.swift ------------------------------------------------------===//
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
import Darwin

func IsPowerOfTwo(_ x: Int) -> Bool { return (x & (x - 1)) == 0 }

// Fast Walsh Hadamard Transform
func WalshTransform(_ data: inout [Double]) {
  assert(IsPowerOfTwo(data.count), "Not a power of two")
  var temp = [Double](repeating: 0, count: data.count)
  var ret = WalshImpl(&data, &temp, 0, data.count)
  for i in 0..<data.count {
    data[i] = ret[i]
  }
}

func Scale(_ data: inout [Double], _ scalar : Double) {
  for i in 0..<data.count {
    data[i] = data[i] * scalar
  }
}

func InverseWalshTransform(_ data: inout [Double]) {
  WalshTransform(&data)
  Scale(&data, Double(1)/Double(data.count))
}

func WalshImpl(_ data: inout [Double], _ temp: inout [Double], _ start: Int, _ size: Int) -> [Double] {
  if (size == 1) { return data }

  let stride = size/2
  for i in 0..<stride {
    temp[start + i]          = data[start + i + stride] + data[start + i]
    temp[start + i + stride] = data[start + i] - data[start + i + stride]
  }

  _ = WalshImpl(&temp, &data, start, stride)
  return WalshImpl(&temp, &data, start + stride, stride)
}

func checkCorrectness() {
  var In : [Double] = [1,0,1,0,0,1,1,0]
  var Out : [Double] = [4,2,0,-2,0,2,0,2]
  var data : [Double] = In
  WalshTransform(&data)
  var mid = data
  InverseWalshTransform(&data)
  for i in 0..<In.count {
    // Check encode.
    CheckResults(abs(data[i] - In[i]) < 0.0001, "Incorrect results in Walsh.")
    // Check decode.
    CheckResults(abs(mid[i] - Out[i]) < 0.0001, "Incorrect results in Walsh.")
  }
}

@inline(never)
public func run_Walsh(_ N: Int) {
  checkCorrectness()

  // Generate data.
  var data2 : [Double] = []
  for i in 0..<1024 {
    data2.append(Double(sin(Float(i))))
  }

  // Transform back and forth.
  for _ in 1...10*N {
    WalshTransform(&data2)
    InverseWalshTransform(&data2)
  }
}

