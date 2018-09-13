//===--- ByteSwap.swift ---------------------------------------------------===//
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

// This test checks performance of Swift byte swap.
// rdar://problem/22151907

import Foundation
import TestsUtils

public let ByteSwap = BenchmarkInfo(
  name: "ByteSwap",
  runFunction: run_ByteSwap,
  tags: [.validation, .algorithm])

// a naive O(n) implementation of byteswap.
@inline(never)
func byteswap_n(_ a: UInt64) -> UInt64 {
  return ((a & 0x00000000000000FF) &<< 56) |
         ((a & 0x000000000000FF00) &<< 40) |
         ((a & 0x0000000000FF0000) &<< 24) |
         ((a & 0x00000000FF000000) &<<  8) |
         ((a & 0x000000FF00000000) &>>  8) |
         ((a & 0x0000FF0000000000) &>> 24) |
         ((a & 0x00FF000000000000) &>> 40) |
         ((a & 0xFF00000000000000) &>> 56)
}

// a O(logn) implementation of byteswap.
@inline(never)
func byteswap_logn(_ a: UInt64) -> UInt64 {
  var a = a
  a = (a & 0x00000000FFFFFFFF) << 32 | (a & 0xFFFFFFFF00000000) >> 32
  a = (a & 0x0000FFFF0000FFFF) << 16 | (a & 0xFFFF0000FFFF0000) >> 16
  a = (a & 0x00FF00FF00FF00FF) << 8  | (a & 0xFF00FF00FF00FF00) >> 8
  return a
}

@inline(never)
public func run_ByteSwap(_ N: Int) {
  var s: UInt64 = 0
  for _ in 1...10000*N {
    // Check some results.
    let x : UInt64 = UInt64(getInt(0))
    s = s &+ byteswap_logn(byteswap_n(x &+ 2457))
          &+ byteswap_logn(byteswap_n(x &+ 9129))
          &+ byteswap_logn(byteswap_n(x &+ 3333))
  }
  CheckResults(s == Int(2457 &+ 9129 &+ 3333) &* 10000 &* N)
}
