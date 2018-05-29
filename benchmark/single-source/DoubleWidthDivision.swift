//===--- DoubleWidthDivision.swift ----------------------------------------===//
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

// This test checks performance of division using DoubleWidth.

// FIXME: This needs to be disabled with an #if because it runs into pathological
// compile time and code size problems.
// [SR-6947] DoubleWidth compile time.
// import Foundation
import TestsUtils

public let DoubleWidthDivision = BenchmarkInfo(
  name: "DoubleWidthDivision",
  runFunction: disabled,
  tags: [.validation, .algorithm]
)
public func disabled(_ N: Int) {}

#if false

private typealias Int128 = DoubleWidth<Int64>
private typealias Int256 = DoubleWidth<Int128>
private typealias Int512 = DoubleWidth<Int256>
private typealias Int1024 = DoubleWidth<Int512>

@inline(never)
public func run_DoubleWidthDivision(_ N: Int) {
  var sum = 0
  for _ in 1...5*N {
    let (q, r) =
      (Int128(Int64.max) * 16)
        .quotientAndRemainder(dividingBy: numericCast(getInt(16)))
    sum += Int(q * r)

    let (q1, r1) =
      (40 as Int128).dividingFullWidth(
        (high: numericCast(getInt(0)), low: numericCast(getInt(240))))
    sum += Int(q1 * r1)

    let x =
      DoubleWidth<DoubleWidth<DoubleWidth<Int8>>>(
        Int64.max / numericCast(getInt(4)))
    let y = DoubleWidth<DoubleWidth<Int8>>(Int32.max)
    let (q2, r2) = y.dividingFullWidth((x.high, x.low))
    sum += Int(q2 - r2)

    let xx = Int1024(x)
    let yy = Int512(y)
    let (q3, r3) = yy.dividingFullWidth((xx.high, xx.low))
    sum -= Int(q3 - r3)
  }
  CheckResults(sum == 0)
}
#endif
