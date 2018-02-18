// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let OffsetSubscript = TestSuite("OffsetSubscript")

OffsetSubscript.test("closed-range") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: 2...4]
  
  expectEqualSequence(y, [54, 1, 3])
}

OffsetSubscript.test("half-open-range") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: 5..<7]
  
  expectEqualSequence(y, [4, 5])
}

OffsetSubscript.test("partial-range-upto") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: ..<4]
  
  expectEqualSequence(y, [2, 3, 54, 1])
}

OffsetSubscript.test("partial-range-through") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: ...5]
  
  expectEqualSequence(y, [2, 3, 54, 1, 3, 4])
}

OffsetSubscript.test("partial-range-from") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: 3...]
  
  expectEqualSequence(y, [1, 3, 4, 5, 6, 8])
}

OffsetSubscript.test("offsetting-a-subsequence") {
  let x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  let y = x[offset: 2...7]
  let z = y[offset: 0...2]
  
  expectEqualSequence(y, [54, 1, 3, 4, 5, 6])
  expectEqualSequence(z, [54, 1, 3])
}

OffsetSubscript.test("replace-range") {
  var x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  x[offset: 3...6] = []
  expectEqualSequence(x, [2, 3, 54, 6, 8])
  
  x[offset: 1..<4] = [3, 4, 5, 6, 7, 8, 9]
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<0] = []
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<x.count] = []
  expectEqualSequence(x, [])
}

OffsetSubscript.test("inplace-mutation") {
  var x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  x[offset: 3...6] = []
  expectEqualSequence(x, [2, 3, 54, 6, 8])
  
  x[offset: 1..<4] = [3, 4, 5, 6, 7, 8, 9]
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<0] = []
  expectEqualSequence(x, [2, 3, 4, 5, 6, 7, 8, 9, 8])
  
  x[offset: 0..<x.count] = []
  expectEqualSequence(x, [])
}

OffsetSubscript.test("relativeAsOffset") {
  var x = [2, 3, 54, 1, 3, 4, 5, 6, 8]
  
  let r0 = (1...5)._relativeAsOffset(to: x)
  let i0s = x.index(x.startIndex, offsetBy: 1)
  let i0e = x.index(x.startIndex, offsetBy: 6)
  let i0: Range = i0s..<i0e
  expectEqual(r0, i0)

  let r1 = (1..<7)._relativeAsOffset(to: x)
  let i1s = x.index(x.startIndex, offsetBy: 1)
  let i1e = x.index(x.startIndex, offsetBy: 7)
  let i1: Range = i1s..<i1e
  expectEqual(r1, i1)

  let r2 = (3...)._relativeAsOffset(to: x)
  let i2: Range = x.index(x.startIndex, offsetBy: 3)..<x.endIndex
  expectEqual(r2, i2)

  let r3 = (...6)._relativeAsOffset(to: x)
  let i3: Range = x.startIndex..<x.index(x.startIndex, offsetBy: 7)
  expectEqual(r3, i3)

  let r4 = (..<6)._relativeAsOffset(to: x)
  let i4: Range = x.startIndex..<x.index(x.startIndex, offsetBy: 6)
  expectEqual(r4, i4)


  let r5 = (2..<6)._relativeAsOffset(to: x)
  let i5s = x.index(x.startIndex, offsetBy: 2)
  let i5e = x.index(x.startIndex, offsetBy: 6)
  let i5: Range = i5s..<i5e
  expectEqual(r5, i5)

  let y = x[i5]
  let r6 = (2..<6)._relativeAsOffset(to: y)
  let i6s = y.index(y.startIndex, offsetBy: 2)
  let i6e = y.index(y.startIndex, offsetBy: 6)
  let i6: Range = i6s..<i6e
  expectEqual(r6, i6)
}

runAllTests()