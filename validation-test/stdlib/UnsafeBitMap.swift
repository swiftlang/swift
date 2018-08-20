// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

var UnsafeBitMapTests = TestSuite("UnsafeBitMap")

UnsafeBitMapTests.test("wordIndex(_:), bitIndex(_:)") {
#if arch(i386) || arch(arm)
  for i in 0...31 {
    expectEqual(0, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
  for i in 32...63 {
    expectEqual(1, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i - 32, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
  for i in 64...95 {
    expectEqual(2, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i - 64, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
  for i in 96...127 {
    expectEqual(3, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i - 96, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  for i in 0...63 {
    expectEqual(0, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
  for i in 64...127 {
    expectEqual(1, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i - 64, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
  for i in 192...255 {
    expectEqual(3, _UnsafeBitMap.wordIndex(i), "i=\(i)")
    expectEqual(i - 192, Int(_UnsafeBitMap.bitIndex(i)), "i=\(i)")
  }
#else
  _UnimplementedError()
#endif
}

UnsafeBitMapTests.test("sizeInWords(forSizeInBits:)") {
  expectEqual(0, _UnsafeBitMap.sizeInWords(forSizeInBits: 0))
#if arch(i386) || arch(arm)
  for i in 1...32 {
    expectEqual(1, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
  for i in 33...64 {
    expectEqual(2, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
  for i in 65...96 {
    expectEqual(3, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
  for i in 97...128 {
    expectEqual(4, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  for i in 1...64 {
    expectEqual(1, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
  for i in 65...128 {
    expectEqual(2, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
  for i in 193...256 {
    expectEqual(4, _UnsafeBitMap.sizeInWords(forSizeInBits: i), "i=\(i)")
  }
#else
  _UnimplementedError()
#endif
}

let sizes = [
  0, 1, 2, 3, 7, 8, 9, 31, 32, 33, 48, 63, 64, 65, 127, 128, 129, 1024, 10000
]

func make(sizeInBits: Int) -> _UnsafeBitMap {
  let sizeInWords = _UnsafeBitMap.sizeInWords(forSizeInBits: sizeInBits)
  let storage = UnsafeMutablePointer<UInt>.allocate(capacity: sizeInWords)
  let bitMap = _UnsafeBitMap(storage: storage, bitCount: sizeInBits)
  expectEqual(sizeInWords, bitMap.numberOfWords)
  return bitMap
}

UnsafeBitMapTests.test("initializeToZero()")
  .forEach(in: sizes) {
  sizeInBits in
  let bitMap = make(sizeInBits: sizeInBits)
  defer { bitMap.values.deallocate() }

  bitMap.initializeToZero()
  for i in 0..<sizeInBits {
    expectEqual(false, bitMap[i])
  }
}

UnsafeBitMapTests.test("subscript")
  .forEach(in: sizes) {
  sizeInBits in
  let bitMap = make(sizeInBits: sizeInBits)
  defer { bitMap.values.deallocate() }

  if sizeInBits != 0 {
    bitMap.initializeToZero()
    let index = 7882627 % sizeInBits
    bitMap[index] = true
    for i in 0..<sizeInBits {
      expectEqual(i == index, bitMap[i])
    }
  }

  if sizeInBits < 500 {
    for i in 0..<sizeInBits {
      bitMap.initializeToZero()
      bitMap[i] = true
      for j in 0..<sizeInBits {
        expectEqual(i == j, bitMap[j])
      }
    }
  }
}

runAllTests()
