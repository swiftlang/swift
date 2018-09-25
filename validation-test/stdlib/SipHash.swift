// RUN: %target-run-stdlib-swiftgyb
// REQUIRES: executable_test

import StdlibUnittest

let SipHashTests = TestSuite("SipHashTests")

extension Hasher {
  typealias HashValue = Int
}
extension _SipHash13 {
  typealias HashValue = UInt64
}
extension _SipHash24 {
  typealias HashValue = UInt64
}

struct SipHashTest {
  let input: [UInt8]
  let seed: (UInt64, UInt64)
  let output: UInt64

  /// Test vector from the reference C implementation.
  ///
  /// SipHash output with
  ///
  ///     seed = 00 01 02 ...
  ///
  /// and
  ///
  ///     input = (empty string)
  ///     input = 00 (1 byte)
  ///     input = 00 01 (2 bytes)
  ///     input = 00 01 02 (3 bytes)
  ///     ...
  ///     input = 00 01 02 ... 3e (63 bytes)
  init(referenceVectorIndex i: Int, output: UInt64) {
    self.input = Array(0..<UInt8(i))
    self.seed = (0x07060504_03020100, 0x0F0E0D0C_0B0A0908)
    self.output = output
  }

  init(input: [UInt8], seed: (UInt64, UInt64), output: UInt64) {
    self.input = input
    self.seed = seed
    self.output = output
  }
}

let sipHash24Tests: [SipHashTest] = [
  // Test vectors from the reference C implementation, which was released
  // to public domain by:
  //
  // * Jean-Philippe Aumasson <jeanphilippe.aumasson@gmail.com>
  // * Daniel J. Bernstein <djb@cr.yp.to>
  SipHashTest(referenceVectorIndex: 0, output: 0x726fdb47dd0e0e31),
  SipHashTest(referenceVectorIndex: 1, output: 0x74f839c593dc67fd),
  SipHashTest(referenceVectorIndex: 2, output: 0x0d6c8009d9a94f5a),
  SipHashTest(referenceVectorIndex: 3, output: 0x85676696d7fb7e2d),
  SipHashTest(referenceVectorIndex: 4, output: 0xcf2794e0277187b7),
  SipHashTest(referenceVectorIndex: 5, output: 0x18765564cd99a68d),
  SipHashTest(referenceVectorIndex: 6, output: 0xcbc9466e58fee3ce),
  SipHashTest(referenceVectorIndex: 7, output: 0xab0200f58b01d137),
  SipHashTest(referenceVectorIndex: 8, output: 0x93f5f5799a932462),
  SipHashTest(referenceVectorIndex: 9, output: 0x9e0082df0ba9e4b0),
  SipHashTest(referenceVectorIndex: 10, output: 0x7a5dbbc594ddb9f3),
  SipHashTest(referenceVectorIndex: 11, output: 0xf4b32f46226bada7),
  SipHashTest(referenceVectorIndex: 12, output: 0x751e8fbc860ee5fb),
  SipHashTest(referenceVectorIndex: 13, output: 0x14ea5627c0843d90),
  SipHashTest(referenceVectorIndex: 14, output: 0xf723ca908e7af2ee),
  SipHashTest(referenceVectorIndex: 15, output: 0xa129ca6149be45e5),
  SipHashTest(referenceVectorIndex: 16, output: 0x3f2acc7f57c29bdb),
  SipHashTest(referenceVectorIndex: 17, output: 0x699ae9f52cbe4794),
  SipHashTest(referenceVectorIndex: 18, output: 0x4bc1b3f0968dd39c),
  SipHashTest(referenceVectorIndex: 19, output: 0xbb6dc91da77961bd),
  SipHashTest(referenceVectorIndex: 20, output: 0xbed65cf21aa2ee98),
  SipHashTest(referenceVectorIndex: 21, output: 0xd0f2cbb02e3b67c7),
  SipHashTest(referenceVectorIndex: 22, output: 0x93536795e3a33e88),
  SipHashTest(referenceVectorIndex: 23, output: 0xa80c038ccd5ccec8),
  SipHashTest(referenceVectorIndex: 24, output: 0xb8ad50c6f649af94),
  SipHashTest(referenceVectorIndex: 25, output: 0xbce192de8a85b8ea),
  SipHashTest(referenceVectorIndex: 26, output: 0x17d835b85bbb15f3),
  SipHashTest(referenceVectorIndex: 27, output: 0x2f2e6163076bcfad),
  SipHashTest(referenceVectorIndex: 28, output: 0xde4daaaca71dc9a5),
  SipHashTest(referenceVectorIndex: 29, output: 0xa6a2506687956571),
  SipHashTest(referenceVectorIndex: 30, output: 0xad87a3535c49ef28),
  SipHashTest(referenceVectorIndex: 31, output: 0x32d892fad841c342),
  SipHashTest(referenceVectorIndex: 32, output: 0x7127512f72f27cce),
  SipHashTest(referenceVectorIndex: 33, output: 0xa7f32346f95978e3),
  SipHashTest(referenceVectorIndex: 34, output: 0x12e0b01abb051238),
  SipHashTest(referenceVectorIndex: 35, output: 0x15e034d40fa197ae),
  SipHashTest(referenceVectorIndex: 36, output: 0x314dffbe0815a3b4),
  SipHashTest(referenceVectorIndex: 37, output: 0x027990f029623981),
  SipHashTest(referenceVectorIndex: 38, output: 0xcadcd4e59ef40c4d),
  SipHashTest(referenceVectorIndex: 39, output: 0x9abfd8766a33735c),
  SipHashTest(referenceVectorIndex: 40, output: 0x0e3ea96b5304a7d0),
  SipHashTest(referenceVectorIndex: 41, output: 0xad0c42d6fc585992),
  SipHashTest(referenceVectorIndex: 42, output: 0x187306c89bc215a9),
  SipHashTest(referenceVectorIndex: 43, output: 0xd4a60abcf3792b95),
  SipHashTest(referenceVectorIndex: 44, output: 0xf935451de4f21df2),
  SipHashTest(referenceVectorIndex: 45, output: 0xa9538f0419755787),
  SipHashTest(referenceVectorIndex: 46, output: 0xdb9acddff56ca510),
  SipHashTest(referenceVectorIndex: 47, output: 0xd06c98cd5c0975eb),
  SipHashTest(referenceVectorIndex: 48, output: 0xe612a3cb9ecba951),
  SipHashTest(referenceVectorIndex: 49, output: 0xc766e62cfcadaf96),
  SipHashTest(referenceVectorIndex: 50, output: 0xee64435a9752fe72),
  SipHashTest(referenceVectorIndex: 51, output: 0xa192d576b245165a),
  SipHashTest(referenceVectorIndex: 52, output: 0x0a8787bf8ecb74b2),
  SipHashTest(referenceVectorIndex: 53, output: 0x81b3e73d20b49b6f),
  SipHashTest(referenceVectorIndex: 54, output: 0x7fa8220ba3b2ecea),
  SipHashTest(referenceVectorIndex: 55, output: 0x245731c13ca42499),
  SipHashTest(referenceVectorIndex: 56, output: 0xb78dbfaf3a8d83bd),
  SipHashTest(referenceVectorIndex: 57, output: 0xea1ad565322a1a0b),
  SipHashTest(referenceVectorIndex: 58, output: 0x60e61c23a3795013),
  SipHashTest(referenceVectorIndex: 59, output: 0x6606d7e446282b93),
  SipHashTest(referenceVectorIndex: 60, output: 0x6ca4ecb15c5f91e1),
  SipHashTest(referenceVectorIndex: 61, output: 0x9f626da15c9625f3),
  SipHashTest(referenceVectorIndex: 62, output: 0xe51b38608ef25f57),
  SipHashTest(referenceVectorIndex: 63, output: 0x958a324ceb064572),
  // End of reference test vectors.

  SipHashTest(
    input: [
      0x72, 0xdc, 0xde, 0xd4, 0x6d, 0xb4, 0xc8, 0xa1,
      0xcf, 0x22, 0xe2, 0x7f, 0xe3, 0xf6, 0xe5, 0x6d,
      0x8b, 0x66, 0x0b, 0xaf, 0xba, 0x16, 0x25, 0xf3,
      0x63, 0x8e, 0x69, 0x80, 0xf3, 0x7e, 0xd6, 0xe3,
    ],
    seed: (0xa3432fc680796c34, 0x1173946a79aeaae5),
    output: 0x058b04535972ff2b),
]

let sipHash13Tests: [SipHashTest] = [
  SipHashTest(referenceVectorIndex: 0, output: 0xabac0158050fc4dc),
  SipHashTest(referenceVectorIndex: 1, output: 0xc9f49bf37d57ca93),
  SipHashTest(referenceVectorIndex: 2, output: 0x82cb9b024dc7d44d),
  SipHashTest(referenceVectorIndex: 3, output: 0x8bf80ab8e7ddf7fb),
  SipHashTest(referenceVectorIndex: 4, output: 0xcf75576088d38328),
  SipHashTest(referenceVectorIndex: 5, output: 0xdef9d52f49533b67),
  SipHashTest(referenceVectorIndex: 6, output: 0xc50d2b50c59f22a7),
  SipHashTest(referenceVectorIndex: 7, output: 0xd3927d989bb11140),
  SipHashTest(referenceVectorIndex: 8, output: 0x369095118d299a8e),
  SipHashTest(referenceVectorIndex: 9, output: 0x25a48eb36c063de4),
  SipHashTest(referenceVectorIndex: 10, output: 0x79de85ee92ff097f),
  SipHashTest(referenceVectorIndex: 11, output: 0x70c118c1f94dc352),
  SipHashTest(referenceVectorIndex: 12, output: 0x78a384b157b4d9a2),
  SipHashTest(referenceVectorIndex: 13, output: 0x306f760c1229ffa7),
  SipHashTest(referenceVectorIndex: 14, output: 0x605aa111c0f95d34),
  SipHashTest(referenceVectorIndex: 15, output: 0xd320d86d2a519956),
  SipHashTest(referenceVectorIndex: 16, output: 0xcc4fdd1a7d908b66),
  SipHashTest(referenceVectorIndex: 17, output: 0x9cf2689063dbd80c),
  SipHashTest(referenceVectorIndex: 18, output: 0x8ffc389cb473e63e),
  SipHashTest(referenceVectorIndex: 19, output: 0xf21f9de58d297d1c),
  SipHashTest(referenceVectorIndex: 20, output: 0xc0dc2f46a6cce040),
  SipHashTest(referenceVectorIndex: 21, output: 0xb992abfe2b45f844),
  SipHashTest(referenceVectorIndex: 22, output: 0x7ffe7b9ba320872e),
  SipHashTest(referenceVectorIndex: 23, output: 0x525a0e7fdae6c123),
  SipHashTest(referenceVectorIndex: 24, output: 0xf464aeb267349c8c),
  SipHashTest(referenceVectorIndex: 25, output: 0x45cd5928705b0979),
  SipHashTest(referenceVectorIndex: 26, output: 0x3a3e35e3ca9913a5),
  SipHashTest(referenceVectorIndex: 27, output: 0xa91dc74e4ade3b35),
  SipHashTest(referenceVectorIndex: 28, output: 0xfb0bed02ef6cd00d),
  SipHashTest(referenceVectorIndex: 29, output: 0x88d93cb44ab1e1f4),
  SipHashTest(referenceVectorIndex: 30, output: 0x540f11d643c5e663),
  SipHashTest(referenceVectorIndex: 31, output: 0x2370dd1f8c21d1bc),
  SipHashTest(referenceVectorIndex: 32, output: 0x81157b6c16a7b60d),
  SipHashTest(referenceVectorIndex: 33, output: 0x4d54b9e57a8ff9bf),
  SipHashTest(referenceVectorIndex: 34, output: 0x759f12781f2a753e),
  SipHashTest(referenceVectorIndex: 35, output: 0xcea1a3bebf186b91),
  SipHashTest(referenceVectorIndex: 36, output: 0x2cf508d3ada26206),
  SipHashTest(referenceVectorIndex: 37, output: 0xb6101c2da3c33057),
  SipHashTest(referenceVectorIndex: 38, output: 0xb3f47496ae3a36a1),
  SipHashTest(referenceVectorIndex: 39, output: 0x626b57547b108392),
  SipHashTest(referenceVectorIndex: 40, output: 0xc1d2363299e41531),
  SipHashTest(referenceVectorIndex: 41, output: 0x667cc1923f1ad944),
  SipHashTest(referenceVectorIndex: 42, output: 0x65704ffec8138825),
  SipHashTest(referenceVectorIndex: 43, output: 0x24f280d1c28949a6),
  SipHashTest(referenceVectorIndex: 44, output: 0xc2ca1cedfaf8876b),
  SipHashTest(referenceVectorIndex: 45, output: 0xc2164bfc9f042196),
  SipHashTest(referenceVectorIndex: 46, output: 0xa16e9c9368b1d623),
  SipHashTest(referenceVectorIndex: 47, output: 0x49fb169c8b5114fd),
  SipHashTest(referenceVectorIndex: 48, output: 0x9f3143f8df074c46),
  SipHashTest(referenceVectorIndex: 49, output: 0xc6fdaf2412cc86b3),
  SipHashTest(referenceVectorIndex: 50, output: 0x7eaf49d10a52098f),
  SipHashTest(referenceVectorIndex: 51, output: 0x1cf313559d292f9a),
  SipHashTest(referenceVectorIndex: 52, output: 0xc44a30dda2f41f12),
  SipHashTest(referenceVectorIndex: 53, output: 0x36fae98943a71ed0),
  SipHashTest(referenceVectorIndex: 54, output: 0x318fb34c73f0bce6),
  SipHashTest(referenceVectorIndex: 55, output: 0xa27abf3670a7e980),
  SipHashTest(referenceVectorIndex: 56, output: 0xb4bcc0db243c6d75),
  SipHashTest(referenceVectorIndex: 57, output: 0x23f8d852fdb71513),
  SipHashTest(referenceVectorIndex: 58, output: 0x8f035f4da67d8a08),
  SipHashTest(referenceVectorIndex: 59, output: 0xd89cd0e5b7e8f148),
  SipHashTest(referenceVectorIndex: 60, output: 0xf6f4e6bcf7a644ee),
  SipHashTest(referenceVectorIndex: 61, output: 0xaec59ad80f1837f2),
  SipHashTest(referenceVectorIndex: 62, output: 0xc3b2f6154b6694e0),
  SipHashTest(referenceVectorIndex: 63, output: 0x9d199062b7bbb3a8),

  SipHashTest(
    input: [
      0x72, 0xdc, 0xde, 0xd4, 0x6d, 0xb4, 0xc8, 0xa1,
      0xcf, 0x22, 0xe2, 0x7f, 0xe3, 0xf6, 0xe5, 0x6d,
      0x8b, 0x66, 0x0b, 0xaf, 0xba, 0x16, 0x25, 0xf3,
      0x63, 0x8e, 0x69, 0x80, 0xf3, 0x7e, 0xd6, 0xe3,
    ],
    seed: (0xa3432fc680796c34, 0x1173946a79aeaae5),
    output: 0x4d457d818f46941d),
]

let incrementalPatterns: [[Int]] = [
  [1], [2], [3], [4], [5], [6], [7], [8], [9],
  [15], [16], [17],
  [31], [32], [33],
  [0, 1],
  [1, 2],
  [1, 3, 5],
  [1, 7],
  [1, 9],
  [7, 9],
]

struct Loop<C: Collection>: Sequence, IteratorProtocol {
  var base: C
  var iterator: C.Iterator

  init(_ base: C) {
    self.base = base
    self.iterator = base.makeIterator()
  }

  mutating func next() -> C.Element? {
    if let element = iterator.next() {
      return element
    }
    iterator = base.makeIterator()
    return iterator.next()
  }
}

% for (Self, tests) in [
%   ('Hasher', 'sipHash13Tests'),
%   ('_SipHash13', 'sipHash13Tests'),
%   ('_SipHash24', 'sipHash24Tests')
% ]:

SipHashTests.test("${Self}/combine(UnsafeRawBufferPointer)")
  .forEach(in: ${tests}) { test in
  var hasher = ${Self}(_rawSeed: test.seed)
  test.input.withUnsafeBytes { hasher.combine(bytes: $0) }
  let hash = hasher.finalize()
  expectEqual(${Self}.HashValue(truncatingIfNeeded: test.output), hash)
}

SipHashTests.test("${Self}/combine(UnsafeRawBufferPointer)/pattern")
  .forEach(in: cartesianProduct(${tests}, incrementalPatterns)) { test_ in
  let (test, pattern) = test_

  var hasher = ${Self}(_rawSeed: test.seed)
  var chunkSizes = Loop(pattern).makeIterator()
  var startIndex = 0
  while startIndex != test.input.endIndex {
    let chunkSize = min(
      chunkSizes.next()!,
      test.input.endIndex - startIndex)
    let slice = test.input[startIndex..<(startIndex+chunkSize)]
    slice.withUnsafeBytes { hasher.combine(bytes: $0) }
    startIndex += chunkSize
  }
  let hash = hasher.finalize()
  expectEqual(${Self}.HashValue(truncatingIfNeeded: test.output), hash)
}

% for data_type in ['UInt', 'UInt64', 'UInt32', 'UInt16', 'UInt8']:
SipHashTests.test("${Self}._combine(${data_type})")
  .forEach(in: ${tests}) { test in

  var hasher = ${Self}(_rawSeed: test.seed)

  // Load little-endian chunks and combine them into the hasher.
  let bitWidth = ${data_type}.bitWidth
  var i = 0
  var count = 0
  var chunk: ${data_type} = 0
  while i < test.input.count {
    chunk |= ${data_type}(test.input[i]) << (8 * count)
    i += 1
    count += 1
    if 8 * count == bitWidth {
      hasher._combine(chunk)
      count = 0
      chunk = 0
    }
  }
  // Combine tail bytes.
  if count > 0 {
    hasher._combine(bytes: UInt64(truncatingIfNeeded: chunk), count: count)
  }

  let hash = hasher.finalize()
  expectEqual(${Self}.HashValue(truncatingIfNeeded: test.output), hash)
}
% end

SipHashTests.test("${Self}/OperationsAfterFinalize") {
  // Verify that finalize is nonmutating.
  var hasher1 = ${Self}(_rawSeed: (0, 0))
  hasher1._combine(1 as UInt8)
  _ = hasher1.finalize()
  // Hasher is now consumed. The operations below are illegal, but this isn't
  // currently enforced. Check that the behavior matches that of a nonmutating
  // function.
  hasher1._combine(2 as UInt16)
  let hash1a = hasher1.finalize()
  let hash1b = hasher1.finalize()
  expectEqual(hash1a, hash1b)

  var hasher2 = ${Self}(_rawSeed: (0, 0))
  hasher2._combine(1 as UInt8)
  hasher2._combine(2 as UInt16)
  let hash2 = hasher2.finalize()
  expectEqual(hash1a, hash2)
}
% end

runAllTests()

