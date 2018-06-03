//===--- RelativeOffset.swift ---------------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let RelativeOffset = TestSuite("RelativeOffset")

RelativeOffset.test("closed-range") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y = x[offset: 2...4]
  expectEqualSequence(y, [54, 1, 3])

  let z = x[offset: (-4)...-2]
  expectEqualSequence(z, [4, 5, 6])
}

RelativeOffset.test("half-open-range") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y = x[offset: 5..<7]
  expectEqualSequence(y, [4, 5])

  let z = x[offset: (-7)..<5]
  expectEqualSequence(z, [54, 1, 3])
}

RelativeOffset.test("partial-range-upto") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y = x[offset: ..<4]
  expectEqualSequence(y, [2, 3, 54, 1])

  let z = x[offset: ..<-4]
  expectEqualSequence(z, [2, 3, 54, 1, 3])
}

RelativeOffset.test("partial-range-through") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y = x[offset: ...5]
  expectEqualSequence(y, [2, 3, 54, 1, 3, 4])

  let z = x[offset: ...-5]
  expectEqualSequence(z, [2, 3, 54, 1, 3])
}

RelativeOffset.test("partial-range-from") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y = x[offset: 3...]
  expectEqualSequence(y, [1, 3, 4, 5, 6, 8])

  let z = x[offset: (-3)...]
  expectEqualSequence(z, [5, 6, 8])
}

RelativeOffset.test("offsetting-a-subsequence") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]

  let y0 = x[offset: 2...7]
  let z0 = y0[offset: 0...2]
  expectEqualSequence(y0, [54, 1, 3, 4, 5, 6])
  expectEqualSequence(z0, [54, 1, 3])

  let y1 = x[offset: (-5)...-2]
  let z1 = y1[offset: 1...3]
  expectEqualSequence(y1, [3, 4, 5, 6])
  expectEqualSequence(z1, [4, 5, 6])
}

RelativeOffset.test("replace-range") {
  var x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  x[offset: 3...6] = []
  expectEqualSequence(x, [2, 3, 54, 6, 8])
  
  x[offset: 1..<4] = [3, 4, 5, 6, 7, 8, 9]
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<0] = []
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<x.count] = []
  expectEqualSequence(x, [])

  var y = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  y[offset: ...-6] = []
  expectEqualSequence(y, [3, 4, 5, 6, 8])
  
  y[offset: (-4)...-2] = [0, 1, 1, 0, 2, 3]
  expectEqualSequence(y, [3, 0, 1, 1, 0, 2, 3, 8])
  
  y[offset: 0..<0] = []
  expectEqualSequence(y, [3, 0, 1, 1, 0, 2, 3, 8])
  
  y[offset: 0...-1] = []
  expectEqualSequence(y, [])
}

RelativeOffset.test("replace-string-range") {
  var x = "Hello, Swift!"

  x[offset: 7...11] = "World"
  expectEqual(x, "Hello, World!")

  x[offset: ...6] = ""
  expectEqual(x, "World!")

  x[offset: -1...-1] = " 42!"
  expectEqual(x, "World 42!")
}

RelativeOffset.test("relativeAsOffset") {
  var x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  
  let r0 = (1...5).relativeOffsetRange()
  let i0 = RelativeOffsetRange(1, 6)
  expectEqual(r0, i0)

  let r1 = (1..<7).relativeOffsetRange()
  let i1 = RelativeOffsetRange(1, 7)
  expectEqual(r1, i1)

  let r2 = (3...).relativeOffsetRange()
  let i2 = RelativeOffsetRange(3, nil)
  expectEqual(r2, i2)

  let r3 = (...6).relativeOffsetRange()
  let i3 = RelativeOffsetRange(0, 7)
  expectEqual(r3, i3)

  let r4 = (..<6).relativeOffsetRange()
  let i4 = RelativeOffsetRange(0, 6)
  expectEqual(r4, i4)


  let r5 = (2..<-6).relativeOffsetRange()
  let i5 = RelativeOffsetRange(2, -6)
  expectEqual(r5, i5)

  let r6 = (2..<-(-6)).relativeOffsetRange()
  let i6 = RelativeOffsetRange(2, 6)
  expectEqual(r6, i6)

  let r7 = ((-2)..<-6).relativeOffsetRange()
  let i7 = RelativeOffsetRange(-2, -6)
  expectEqual(r7, i7)
}

runAllTests()