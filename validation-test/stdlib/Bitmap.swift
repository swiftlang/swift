// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

var BitmapTests = TestSuite("Bitmap")

BitmapTests.test("Word.bitWidth") {
  expectEqual(_Bitmap.Word.bitWidth, UInt.bitWidth)
}

BitmapTests.test("Word._split(_:)") {
#if arch(i386) || arch(arm)
  for i in 0...31 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(0, comps.word, "i=\(i)")
    expectEqual(i, comps.bit, "i=\(i)")
  }
  for i in 32...63 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(1, comps.word, "i=\(i)")
    expectEqual(i - 32, comps.bit, "i=\(i)")
  }
  for i in 64...95 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(2, comps.word, "i=\(i)")
    expectEqual(i - 64, comps.bit, "i=\(i)")
  }
  for i in 96...127 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(3, comps.word, "i=\(i)")
    expectEqual(i - 96, comps.bit, "i=\(i)")
  }
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  for i in 0...63 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(0, comps.word, "i=\(i)")
    expectEqual(i, comps.bit, "i=\(i)")
  }
  for i in 64...127 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(1, comps.word, "i=\(i)")
    expectEqual(i - 64, comps.bit, "i=\(i)")
  }
  for i in 128...191 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(2, comps.word, "i=\(i)")
    expectEqual(i - 128, comps.bit, "i=\(i)")
  }
  for i in 192...255 {
    let comps = _Bitmap.Word._split(i)
    expectEqual(3, comps.word, "i=\(i)")
    expectEqual(i - 192, comps.bit, "i=\(i)")
  }
#else
  _UnimplementedError()
#endif
}

BitmapTests.test("Word._uncheckedContains(_:)") {
  typealias Word = _Bitmap.Word
  for i in 0 ..< Word.bitWidth {
    expectEqual(Word(0)._uncheckedContains(i), false, "i=\(i)")
    expectEqual(Word(1)._uncheckedContains(i), i == 0, "i=\(i)")
    expectEqual(Word(2)._uncheckedContains(i), i == 1, "i=\(i)")
    expectEqual(Word(4)._uncheckedContains(i), i == 2, "i=\(i)")
    expectEqual(Word(8)._uncheckedContains(i), i == 3, "i=\(i)")
    expectEqual(Word(15)._uncheckedContains(i), i <= 3, "i=\(i)")
    expectEqual(Word(~1)._uncheckedContains(i), i != 0, "i=\(i)")
    expectEqual(Word(~2)._uncheckedContains(i), i != 1, "i=\(i)")
    expectEqual(Word(~4)._uncheckedContains(i), i != 2, "i=\(i)")
    expectEqual(Word(~8)._uncheckedContains(i), i != 3, "i=\(i)")
    expectEqual(Word(~15)._uncheckedContains(i), i > 3, "i=\(i)")
    expectEqual(Word(UInt.max)._uncheckedContains(i), true, "i=\(i)")
  }
}

BitmapTests.test("Word._uncheckedInsert(_:)") {
  typealias Word = _Bitmap.Word
  var word = Word(0)
  for i in 0 ..< Word.bitWidth {
    expectFalse(word._uncheckedContains(i))
    expectTrue(word._uncheckedInsert(i))
    expectTrue(word._uncheckedContains(i))
    expectFalse(word._uncheckedInsert(i))
  }
  expectEqual(word._value, UInt.max)
}

BitmapTests.test("Word._uncheckedRemove(_:)") {
  typealias Word = _Bitmap.Word
  var word = Word(UInt.max)
  for i in 0 ..< Word.bitWidth {
    expectTrue(word._uncheckedContains(i))
    expectTrue(word._uncheckedRemove(i))
    expectFalse(word._uncheckedContains(i))
    expectFalse(word._uncheckedRemove(i))
  }
  expectEqual(word._value, 0)
}

BitmapTests.test("Word.count") {
  typealias Word = _Bitmap.Word
  var word = Word(0)
  for i in 0 ..< Word.bitWidth {
    expectEqual(word.count, i)
    _ = word._uncheckedInsert(i)
    expectEqual(word.count, i + 1)
  }
  for i in 0 ..< Word.bitWidth {
    expectEqual(word.count, Word.bitWidth - i)
    _ = word._uncheckedRemove(i)
    expectEqual(word.count, Word.bitWidth - i - 1)
  }
}

BitmapTests.test("Word.makeIterator()") {
  typealias Word = _Bitmap.Word
  expectEqual(Array(Word(0)), [])
  expectEqual(Array(Word(1)), [0])
  expectEqual(Array(Word(2)), [1])
  expectEqual(Array(Word(3)), [0, 1])
  expectEqual(Array(Word(42)), [1, 3, 5])
  expectEqual(Array(Word(UInt.max)), Array(0..<UInt.bitWidth))
}

BitmapTests.test("Storage.wordCount(forBitCount:)") {
  typealias Storage = _Bitmap.Storage
  expectEqual(0, Storage.wordCount(forBitCount: 0))
#if arch(i386) || arch(arm)
  for i in 1...32 {
    expectEqual(1, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
  for i in 33...64 {
    expectEqual(2, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
  for i in 65...96 {
    expectEqual(3, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
  for i in 97...128 {
    expectEqual(4, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  for i in 1...64 {
    expectEqual(1, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
  for i in 65...128 {
    expectEqual(2, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
  for i in 193...256 {
    expectEqual(4, Storage.wordCount(forBitCount: i), "i=\(i)")
  }
#else
  _UnimplementedError()
#endif
}

let bitCounts = [
  0, 1, 2, 3, 7, 8, 9, 31, 32, 33, 48, 63, 64, 65, 127, 128, 129, 1024, 10000
]

let primes: Set<Int> = [
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
  73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
  157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
  239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
  331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
  421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499
]

BitmapTests.test("_Bitmap:InitializedToZero")
.forEach(in: bitCounts) { bitCount in
  let bitmap = _Bitmap(bitCount: bitCount)
  expectEqual(bitmap.count, 0)
  for bit in 0 ..< bitCount {
    expectFalse(bitmap.contains(bit))
  }
}

BitmapTests.test("_Bitmap:Allocation")
.forEach(in: bitCounts) { bitCount in
  let bitmap = _Bitmap(bitCount: bitCount)
  expectEqual(bitmap._storage != nil, bitCount > _Bitmap.Word.bitWidth)
}

BitmapTests.test("_Bitmap.contains(_:)")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  for p in primes where p < bitCount {
    expectFalse(bitmap.contains(p), "\(p)")
    _ = bitmap.insert(p)
    expectTrue(bitmap.contains(p), "\(p)")
  }
  for i in 0 ..< bitCount {
    expectEqual(bitmap.contains(i), primes.contains(i), "\(i)")
  }
}

BitmapTests.test("_Bitmap.insert(_:)")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  for p in primes where p < bitCount {
    let (inserted, member) = bitmap.insert(p)
    expectTrue(inserted, "\(p)")
    expectEqual(member, p, "\(p)")
  }
  for p in primes where p < bitCount {
    let (inserted, member) = bitmap.insert(p)
    expectFalse(inserted, "\(p)")
    expectEqual(member, p, "\(p)")
  }
  for i in 0 ..< bitCount {
    expectEqual(bitmap.contains(i), primes.contains(i), "\(i)")
  }
}

BitmapTests.test("_Bitmap.insert(_:):COW")
.forEach(in: bitCounts) { bitCount in
  for bit in primes where bit < bitCount {
    var bitmap = _Bitmap(bitCount: bitCount)
    let copy = bitmap

    bitmap.insert(bit)
    expectTrue(bitmap.contains(bit), "\(bit)")
    expectFalse(copy.contains(bit), "\(bit)")
  }
}

BitmapTests.test("_Bitmap.remove(_:)")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  for i in 0 ..< bitCount {
    bitmap.insert(i)
  }
  for bit in primes where bit < bitCount {
    expectEqual(bitmap.remove(bit), bit, "\(bit)")
  }
  for bit in primes where bit < bitCount {
    expectNil(bitmap.remove(bit), "\(bit)")
  }
  for i in 0 ..< bitCount {
    expectEqual(bitmap.contains(i), !primes.contains(i), "\(i)")
  }
}

BitmapTests.test("_Bitmap.remove(_:):COW")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  for bit in primes where bit < bitCount {
    bitmap.insert(bit)
  }
  for bit in primes where bit < bitCount {
    var copy = bitmap
    expectEqual(copy.remove(bit), bit, "\(bit)")
    expectFalse(copy.contains(bit), "\(bit)")
    expectTrue(bitmap.contains(bit), "\(bit)")
  }
}

BitmapTests.test("_Bitmap.count")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  var c = 0
  for bit in primes where bit < bitCount {
    bitmap.insert(bit)
    c += 1
  }
  expectEqual(c, bitmap.count)
}

BitmapTests.test("_Bitmap.makeIterator()")
.forEach(in: bitCounts) { bitCount in
  var bitmap = _Bitmap(bitCount: bitCount)
  let bits = primes.filter { $0 < bitCount }
  for b in bits {
    bitmap.insert(b)
  }
  expectEqual(Array(bitmap), bits.sorted())
}

runAllTests()
