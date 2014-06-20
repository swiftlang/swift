// RUN: %target-run-stdlib-swift | FileCheck %s

import Foundation
import Swift

protocol TestableUnicodeCodec : UnicodeCodec {
  typealias CodeUnit : Integer
  class func encodingId() -> NSStringEncoding
  class func name() -> NSString
}

extension UTF8 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF8StringEncoding
  }
  static func name() -> NSString {
    return "UTF8"
  }
}

extension UTF16 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF16LittleEndianStringEncoding
  }
  static func name() -> NSString {
    return "UTF16"
  }
}

extension UTF32 : TestableUnicodeCodec {
  static func encodingId() -> NSStringEncoding {
    return NSUTF32LittleEndianStringEncoding
  }
  static func name() -> NSString {
    return "UTF32"
  }
}

// Backing store for computed var unicodeScalarRanges
var _unicodeScalarRanges  = Array<Range<UInt32>>()

// The valid ranges of Unicode scalar values
var unicodeScalarRanges : Range<UInt32>[] {
  if _unicodeScalarRanges.count == 0 {
    for r in [UInt32(0)..<0xD800, 0xE000..<0xFDD0, 0xFDF0..<0xFFFE] {
      _unicodeScalarRanges.append(r)
    }
    for base in UInt32(0x1)..<0x11 {
      _unicodeScalarRanges.append((base << 16)..<((base << 16)+0xFFFE))
    }
  }
  return _unicodeScalarRanges
}

var unicodeScalarCount : Int {
  var count = 0
  for r in unicodeScalarRanges {
    count += Int(r.endIndex - r.startIndex)
  }
  return count
}

func nthUnicodeScalar(n: UInt32) -> UnicodeScalar {
  var count: UInt32 = 0
  for r in unicodeScalarRanges {
    count += r.endIndex - r.startIndex
    if count > n {
      return UnicodeScalar(r.endIndex - (count - n))
    }
  }
  _preconditionFailure("Index out of range")
}
  
// buffer should have a length >= 4
func nsEncode<CodeUnit>(
  var c: UInt32,
  encoding: NSStringEncoding,
  inout buffer: CodeUnit[],
  inout used: Int
) {
  var s = NSString(
    bytes: &c,
    length: 4,
    encoding: NSUTF32LittleEndianStringEncoding)

  s.getBytes(
      &buffer,
      maxLength: buffer.count,
      usedLength: &used,
      encoding: encoding,
      options: NSStringEncodingConversionOptions(0),
      range: NSRange(location: 0, length: s.length),
    remainingRange: nil)
}

// Convert the given numeric value to a hexidecimal string
func hex<T : Integer>(x: T) -> String {
  return "0x" + _int64ToString(x.toIntMax(), radix: 16)
}

// Convert the given sequence of numeric values to a string
// representing their hexidecimal values
func hex<
  S: Sequence
where
  S.GeneratorType.Element : Integer
>(x: S) -> String {
  var r = "["
  var prefix = ""
  for unit in x {
    r += prefix + hex(unit)
    prefix = ", "
  }
  r += "]"
  return r
}

// A Sink that stores the elements written into an Array that can be
// inspected later.
class ArraySink<T: IntegerLiteralConvertible> : Sink {
  init(capacity: Int) {
    storage = Array(count: capacity, repeatedValue: 0)
  }
  func put(x: T) {
    storage[count++] = x
  }
  func clear() {
    count = 0
  }
  var elements : Slice<T> {
    return storage[0..<count]
  }
  var count = 0
  var storage: T[] = Array()
}

@asmname("random") func random() -> UInt32
@asmname("srandomdev") func srandomdev()

// To avoid swamping the buildbot, by default, test only one out of
// testGroupCount cases, selected at random.  You can manually pass
// "--all" on the command line to test everything.
var testGroupCount = 128
srandomdev()
var testGroup = random() % testGroupCount
var testAll = Process.arguments.count > 0 && Process.arguments[0] == "--all"
var minScalarOrd : Int
var maxScalarOrd : Int

if testAll {
  println("Testing all Unicode scalars")
  minScalarOrd = 0
  maxScalarOrd = unicodeScalarCount
}
else {
  println("Testing Unicode scalar group \(testGroup) of \(testGroupCount)")
  minScalarOrd = unicodeScalarCount * testGroup / testGroupCount
  maxScalarOrd = unicodeScalarCount * (testGroup+1) / testGroupCount
}

class CodecTest<Codec: TestableUnicodeCodec> {
  var used = 0
  typealias CodeUnit = Codec.CodeUnit
  var nsEncodeBuffer: CodeUnit[] = Array(count: 4, repeatedValue: 0)
  var encodeBuffer = ArraySink<CodeUnit>(capacity: 4)

  func testOne(scalar: UnicodeScalar)
  {
    /* Progress reporter
    if (scalar.value % 0x1000) == 0 {
      println("\(hex(scalar.value))")
    }
    */
    
    // Use Cocoa to encode the scalar
    nsEncode(scalar.value, Codec.encodingId(), &nsEncodeBuffer, &used)
    let nsEncoded = nsEncodeBuffer[0..<(used/sizeof(CodeUnit.self))]

    var g = nsEncoded.generate()
    var decoded: UnicodeScalar
    var decoder = Codec()
    switch decoder.decode(&g) {
    case .Result(let us):
      decoded = us
    default:
      fatalError("decoding failed")
    }
    if decoded != scalar {
      println("Decoding failed: \(hex(scalar.value)) => \(hex(nsEncoded)) => \(hex(decoded.value))")
    }
    encodeBuffer.clear()
    Codec.encode(scalar, output: &self.encodeBuffer)
    
    if !equal(nsEncoded, encodeBuffer.elements)  {
      println("Decoding failed: \(hex(nsEncoded)) => \(hex(scalar.value)) => \(hex(encodeBuffer.storage[0]))")
    }
  }
  
  func run() {
    println("testing \(Codec.name())")
    for i in minScalarOrd..<maxScalarOrd {
      testOne(nthUnicodeScalar(UInt32(i)))
    }
    println("done.")
  }
}

srandomdev()

// CHECK: testing UTF8
// CHECK-NEXT: done.
CodecTest<UTF8>().run()

// CHECK: testing UTF16
// CHECK-NEXT: done.
CodecTest<UTF16>().run()

// CHECK: testing UTF32
// CHECK-NEXT: done.
CodecTest<UTF32>().run()

func println(a: UTF8.CodeUnit[]) {
  print("[ ")
  var prefix = ""
  for x in a {
    print("\(prefix)\(x)")
    prefix = ", "
  }
  println(" ]")
}

func println(a: UTF16.CodeUnit[]) {
  print("[ ")
  var prefix = ""
  for x in a {
    print("\(prefix)\(x)")
    prefix = ", "
  }
  println(" ]")
}

func additionalUtf16Tests() {
  // CHECK: additionalUtf16Tests
  println("additionalUtf16Tests")
  // CHECK-NEXT: 1
  println(UTF16.width("x"))
  // CHECK-NEXT: 2
  println(UTF16.width("\U00101010"))

  // CHECK-NEXT: 2
  println(UTF16.width("𝄞"))
  // CHECK-NEXT: true
  println(UTF16.leadSurrogate("𝄞") == 0xD834)
  // CHECK-NEXT: true
  println(UTF16.trailSurrogate("𝄞") == 0xDD1E)

  var u8: UTF8.CodeUnit[] = [ 0, 1, 2, 3, 4, 5 ]
  var u16: UTF16.CodeUnit[] = [ 6, 7, 8, 9, 10, 11 ]

  u16.withUnsafePointerToElements {
    (p16)->() in
    u8.withUnsafePointerToElements {
      (p8)->() in
      // CHECK-NEXT: [ 0, 1, 2, 9, 10, 11 ]
      UTF16.copy(p8, destination: p16, count: 3)
      println(u16)

      // CHECK-NEXT: [ 9, 10, 11, 3, 4, 5 ]
      UTF16.copy(p16 + 3, destination: p8, count: 3)
      println(u8)

      // CHECK-NEXT: [ 0, 1, 2, 0, 1, 2 ]
      UTF16.copy(p16, destination: p16 + 3, count: 3)
      println(u16)
      
      // CHECK-NEXT: [ 9, 10, 11, 9, 10, 11 ]
      UTF16.copy(p8, destination: p8 + 3, count: 3)
      println(u8)
    }
  }

  let (count0, isASCII0) = UTF16.measure(UTF8.self, input: u8.generate(),
      repairIllFormedSequences: false)!
  // CHECK-NEXT: 6 / true
  println("\(count0) / \(isASCII0)")
  
  let (count1, isASCII1) = UTF16.measure(UTF16.self, input: u16.generate(),
      repairIllFormedSequences: false)!
  // CHECK-NEXT: 6 / true
  println("\(count1) / \(isASCII1)")

  // "€" == U+20AC.
  u8 = [0xF0, 0xA4, 0xAD, 0xA2]
  let (count2, isASCII2) = UTF16.measure(UTF8.self, input: u8.generate(),
      repairIllFormedSequences: false)!
  // CHECK-NEXT: 2 / false
  println("\(count2) / \(isASCII2)")  
}
additionalUtf16Tests()

import StdlibUnittest

class EOFCountingGenerator<T> : Generator {
  var array: T[]
  var index: Int = 0
  var numTimesReturnedEOF: Int = 0

  init(_ array: T[]) {
    self.array = array
  }

  func next() -> T? {
    if index == array.count {
      ++numTimesReturnedEOF
      return .None
    }
    return array[index++]
  }
}

func checkDecodeUTF8(
    expectedHead: UInt32[],
    expectedRepairedTail: UInt32[], utf8Str: UInt8[]
) -> AssertionResult {
  if true {
    var decoded: UInt32[] = []
    var g = EOFCountingGenerator(utf8Str)
    transcode(UTF8.self, UTF32.self, g,
        SinkOf {
          decoded += $0
        },
        stopOnError: true)
    expectGE(1, g.numTimesReturnedEOF)
    if expectedHead != decoded {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expectedHead: \(asHex(expectedHead))\n")
          .withDescription("actual:       \(asHex(decoded))")
    }
  }

  if true {
    var expected = expectedHead
    expected += expectedRepairedTail

    var decoded: UInt32[] = []
    var g = EOFCountingGenerator(utf8Str)
    transcode(UTF8.self, UTF32.self, g,
        SinkOf {
          decoded += $0
        },
        stopOnError: false)
    expectEqual(1, g.numTimesReturnedEOF)
    if expected != decoded {
      return assertionFailure()
          .withDescription("\n")
          .withDescription("expected: \(asHex(expected))\n")
          .withDescription("actual:   \(asHex(decoded))")
    }
  }

  return assertionSuccess()
}

var UTF8Decoder = TestCase("UTF8Decoder")

UTF8Decoder.test("Internal/_numTrailingBytes") {
  for i in UInt8(0x00)...UInt8(0x7f) {
    expectEqual(0, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0x80)...UInt8(0xc1) {
    expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xc2)...UInt8(0xdf) {
    expectEqual(1, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xe0)...UInt8(0xef) {
    expectEqual(2, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xf0)...UInt8(0xf4) {
    expectEqual(3, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  for i in UInt8(0xf5)...UInt8(0xfe) {
    expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
  }
  // Separate test for 0xff because of:
  // <rdar://problem/17376512> Range UInt8(0x00)...UInt8(0xff) invokes a
  // runtime trap
  var i = UInt8(0xff)
  expectEqual(4, UTF8._numTrailingBytes(i)) { "i=\(i)" }
}

UTF8Decoder.test("Empty") {
  expectTrue(checkDecodeUTF8([], [], []))
}

UTF8Decoder.test("SmokeTest") {
  //
  // 1-byte sequences
  //

  // U+0041 LATIN CAPITAL LETTER A
  expectTrue(checkDecodeUTF8([ 0x0041 ], [], [ 0x41 ]))

  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  expectTrue(checkDecodeUTF8([ 0x0041, 0x0042 ], [], [ 0x41, 0x42 ]))

  // U+0000 NULL
  // U+0041 LATIN CAPITAL LETTER A
  // U+0042 LATIN CAPITAL LETTER B
  // U+0000 NULL
  expectTrue(checkDecodeUTF8(
      [ 0x0000, 0x0041, 0x0042, 0x0000 ], [],
      [ 0x00, 0x41, 0x42, 0x00 ]))

  //
  // 2-byte sequences
  //

  // U+0283 LATIN SMALL LETTER ESH
  expectTrue(checkDecodeUTF8([ 0x0283 ], [], [ 0xca, 0x83 ]))

  // U+03BA GREEK SMALL LETTER KAPPA
  // U+1F79 GREEK SMALL LETTER OMICRON WITH OXIA
  // U+03C3 GREEK SMALL LETTER SIGMA
  // U+03BC GREEK SMALL LETTER MU
  // U+03B5 GREEK SMALL LETTER EPSILON
  expectTrue(checkDecodeUTF8(
      [ 0x03ba, 0x1f79, 0x03c3, 0x03bc, 0x03b5 ], [],
      [ 0xce, 0xba, 0xe1, 0xbd, 0xb9, 0xcf, 0x83, 0xce, 0xbc, 0xce, 0xb5 ]))

  //
  // 3-byte sequences
  //

  // U+4F8B CJK UNIFIED IDEOGRAPH-4F8B
  // U+6587 CJK UNIFIED IDEOGRAPH-6587
  expectTrue(checkDecodeUTF8(
      [ 0x4f8b, 0x6587 ], [],
      [ 0xe4, 0xbe, 0x8b, 0xe6, 0x96, 0x87 ]))

  // U+D55C HANGUL SYLLABLE HAN
  // U+AE00 HANGUL SYLLABLE GEUL
  expectTrue(checkDecodeUTF8(
      [ 0xd55c, 0xae00 ], [],
      [ 0xed, 0x95, 0x9c, 0xea, 0xb8, 0x80 ]))

  // U+1112 HANGUL CHOSEONG HIEUH
  // U+1161 HANGUL JUNGSEONG A
  // U+11AB HANGUL JONGSEONG NIEUN
  // U+1100 HANGUL CHOSEONG KIYEOK
  // U+1173 HANGUL JUNGSEONG EU
  // U+11AF HANGUL JONGSEONG RIEUL
  expectTrue(checkDecodeUTF8(
      [ 0x1112, 0x1161, 0x11ab, 0x1100, 0x1173, 0x11af ], [],
      [ 0xe1, 0x84, 0x92, 0xe1, 0x85, 0xa1, 0xe1, 0x86, 0xab,
        0xe1, 0x84, 0x80, 0xe1, 0x85, 0xb3, 0xe1, 0x86, 0xaf ]))

  //
  // 4-byte sequences
  //

  // U+E0100 VARIATION SELECTOR-17
  expectTrue(checkDecodeUTF8(
      [ 0x000E0100 ], [],
      [ 0xf3, 0xa0, 0x84, 0x80 ]))
}

UTF8Decoder.test("FirstPossibleSequence") {
  //
  // First possible sequence of a certain length
  //

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // U+0080 PADDING CHARACTER
  expectTrue(checkDecodeUTF8([ 0x0080 ], [], [ 0xc2, 0x80 ]))

  // U+0800 SAMARITAN LETTER ALAF
  expectTrue(checkDecodeUTF8(
      [ 0x0800 ], [],
      [ 0xe0, 0xa0, 0x80 ]))

  // U+10000 LINEAR B SYLLABLE B008 A
  expectTrue(checkDecodeUTF8(
      [ 0x10000 ], [],
      [ 0xf0, 0x90, 0x80, 0x80 ]))

  // U+200000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80, 0x80 ]))

  // U+4000000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80, 0x80 ]))
}

UTF8Decoder.test("LastPossibleSequence") {
  //
  // Last possible sequence of a certain length
  //

  // U+007F DELETE
  expectTrue(checkDecodeUTF8([ 0x007f ], [], [ 0x7f ]))

  // U+07FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0x07ff ], [], [ 0xdf, 0xbf ]))

  // U+FFFF (noncharacter)
  expectTrue(checkDecodeUTF8(
      [ 0xffff ], [],
      [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf, 0xbf ]))

  // U+3FFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf, 0xbf ]))

  // U+7FFFFFFF (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("CodeSpaceBoundaryConditions") {
  //
  // Other boundary conditions
  //

  // U+D7FF (unassigned)
  expectTrue(checkDecodeUTF8([ 0xd7ff ], [], [ 0xed, 0x9f, 0xbf ]))

  // U+E000 (private use)
  expectTrue(checkDecodeUTF8([ 0xe000 ], [], [ 0xee, 0x80, 0x80 ]))

  // U+FFFD REPLACEMENT CHARACTER
  expectTrue(checkDecodeUTF8([ 0xfffd ], [], [ 0xef, 0xbf, 0xbd ]))

  // U+10FFFF (noncharacter)
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

  // U+110000 (invalid)
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80, 0x80 ]))
}

UTF8Decoder.test("UnexpectedContinuationBytes") {
  //
  // Unexpected continuation bytes
  //

  // A sequence of unexpected continuation bytes that don't follow a first
  // byte, every byte is a maximal subpart.

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x80, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0xbf, 0x82, 0xbf, 0xaa ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0 ]))
  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0, 0x8f ]))

  // All continuation bytes (0x80--0xbf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
        0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
        0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
        0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
        0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
        0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
        0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
        0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf ]))
}

UTF8Decoder.test("LonelyStartBytes") {
  //
  // Lonely start bytes
  //

  // Start bytes of 2-byte sequences (0xc0--0xdf).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
        0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
        0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
        0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xc2, 0x20, 0xc3, 0x20,
        0xc4, 0x20, 0xc5, 0x20, 0xc6, 0x20, 0xc7, 0x20,
        0xc8, 0x20, 0xc9, 0x20, 0xca, 0x20, 0xcb, 0x20,
        0xcc, 0x20, 0xcd, 0x20, 0xce, 0x20, 0xcf, 0x20,
        0xd0, 0x20, 0xd1, 0x20, 0xd2, 0x20, 0xd3, 0x20,
        0xd4, 0x20, 0xd5, 0x20, 0xd6, 0x20, 0xd7, 0x20,
        0xd8, 0x20, 0xd9, 0x20, 0xda, 0x20, 0xdb, 0x20,
        0xdc, 0x20, 0xdd, 0x20, 0xde, 0x20, 0xdf, 0x20 ]))

  // Start bytes of 3-byte sequences (0xe0--0xef).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
        0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xe0, 0x20, 0xe1, 0x20, 0xe2, 0x20, 0xe3, 0x20,
        0xe4, 0x20, 0xe5, 0x20, 0xe6, 0x20, 0xe7, 0x20,
        0xe8, 0x20, 0xe9, 0x20, 0xea, 0x20, 0xeb, 0x20,
        0xec, 0x20, 0xed, 0x20, 0xee, 0x20, 0xef, 0x20 ]))

  // Start bytes of 4-byte sequences (0xf0--0xf7).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
        0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf0, 0x20, 0xf1, 0x20, 0xf2, 0x20, 0xf3, 0x20,
        0xf4, 0x20, 0xf5, 0x20, 0xf6, 0x20, 0xf7, 0x20 ]))

  // Start bytes of 5-byte sequences (0xf8--0xfb).
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xf9, 0xfa, 0xfb ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xf8, 0x20, 0xf9, 0x20, 0xfa, 0x20, 0xfb, 0x20 ]))

  // Start bytes of 6-byte sequences (0xfc--0xfd).
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xfd ]))

  expectTrue(checkDecodeUTF8(
      [], [ 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xfc, 0x20, 0xfd, 0x20 ]))
}

UTF8Decoder.test("InvalidStartBytes") {
  //
  // Other bytes (0xc0--0xc1, 0xfe--0xff).
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfe ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, 0xc1, 0xfe, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0xfe, 0xff, 0xff ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfe, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xff, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
      [ 0xc0, 0x20, 0xc1, 0x20, 0xfe, 0x20, 0xff, 0x20 ]))
}

UTF8Decoder.test("MissingContinuationBytes") {
  //
  // Sequences with one continuation byte missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xdf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xc2, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xdf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xa0, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xec, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe1, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xec, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x9f ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x9f, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xee, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xef, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xee, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xef, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0x90, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf1, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf3, 0xbf, 0xbf, 0x41 ]))

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x80, 0x80, 0x41 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x8f, 0xbf, 0x41 ]))

  // Overlong sequences with one trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x9f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with one trailing byte missing.
  // High surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xa0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xac ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xaf ]))
  // Low surrogates
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb4 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xbf ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+1100xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0x90, 0x80 ]))
  // U+13FBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf4, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf5, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf6, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0x80, 0x80 ]))
  // U+1FFBxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf7, 0xbf, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+2000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80, 0x80 ]))
  // U+3FFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10uzzzzz 10zzzyyyy 10yyyyxx 10xxxxxx
  // U+40000xx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80, 0x80 ]))
  // U+7FFFFFxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with two continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f ]))

  // Overlong sequences with two trailing byte missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x8f ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80 ]))

  // Sequences that represent surrogates with two trailing bytes missing.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+110yxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0x90 ]))
  // U+13Fyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf5, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf6, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0x80 ]))
  // U+1FFyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0xbf ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+200yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x88, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf9, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfa, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0x80, 0x80 ]))
  // U+3FFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfb, 0xbf, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+4000yxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80, 0x80 ]))
  // U+7FFFFyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf, 0xbf ]))

  //
  // Sequences with three continuation bytes missing
  //

  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf2 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4 ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80 ]))

  // Ill-formed 4-byte sequences.
  // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+14yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf5 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf6 ]))
  // U+1Cyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf7 ]))

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+20yyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x88 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf9, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfa, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0x80 ]))
  // U+3FCyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0xbf ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+400yyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x84, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0x80, 0x80 ]))
  // U+7FFCyyxx (invalid)
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfd, 0xbf, 0xbf ]))

  //
  // Sequences with four continuation bytes missing
  //

  // Ill-formed 5-byte sequences.
  // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf9 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfa ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))
  // U+3zyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))

  // Broken overlong sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x80 ]))

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x84 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0x80 ]))
  // U+7Fzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0xbf ]))

  //
  // Sequences with five continuation bytes missing
  //

  // Ill-formed 6-byte sequences.
  // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
  // U+uzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfc ]))
  // U+uuzzyyxx (invalid)
  expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfd ]))

  //
  // Consecutive sequences with trailing bytes missing
  //

  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, /**/ 0xfffd, 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, /**/ 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd,
        0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xc0, /**/ 0xe0, 0x80, /**/ 0xf0, 0x80, 0x80,
        0xf8, 0x80, 0x80, 0x80,
        0xfc, 0x80, 0x80, 0x80, 0x80,
        0xdf, /**/ 0xef, 0xbf, /**/ 0xf7, 0xbf, 0xbf,
        0xfb, 0xbf, 0xbf, 0xbf,
        0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("OverlongSequences") {
  //
  // Overlong UTF-8 sequences
  //

  // U+002F SOLIDUS
  expectTrue(checkDecodeUTF8([ 0x002f ], [], [ 0x2f ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0xaf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0xaf ]))

  // U+0000 NULL
  expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

  // Overlong sequences of the above.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x80, 0x80, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

  // Other overlong and ill-formed sequences.
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xbf ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0x80 ]))
  expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xe0, 0x9f, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0x80, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf0, 0x8f, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xf8, 0x87, 0xbf, 0xbf, 0xbf ]))
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xfc, 0x83, 0xbf, 0xbf, 0xbf, 0xbf ]))
}

UTF8Decoder.test("IsolatedSurrogates") {
  // Unicode 6.3.0:
  //
  //    D71.  High-surrogate code point: A Unicode code point in the range
  //    U+D800 to U+DBFF.
  //
  //    D73.  Low-surrogate code point: A Unicode code point in the range
  //    U+DC00 to U+DFFF.

  // Note: U+E0100 is <DB40 DD00> in UTF16.

  // High surrogates

  // U+D800
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80 ]))
  expectTrue(checkDecodeUTF8(
      [ 0x0041 ],
      [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
      [ 0x41, 0xed, 0xa0, 0x80, 0x41 ]))

  // U+DB40
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0 ]))

  // U+DBFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf ]))

  // Low surrogates

  // U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb0, 0x80 ]))

  // U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xb4, 0x80 ]))

  // U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("SurrogatePairs") {
  // Surrogate pairs

  // U+D800 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb0, 0x80 ]))

  // U+D800 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xb4, 0x80 ]))

  // U+D800 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xa0, 0x80, 0xed, 0xbf, 0xbf ]))

  // U+DB40 U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb0, 0x80 ]))

  // U+DB40 U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xb4, 0x80 ]))

  // U+DB40 U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xac, 0xa0, 0xed, 0xbf, 0xbf ]))

  // U+DBFF U+DC00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb0, 0x80 ]))

  // U+DBFF U+DD00
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xb4, 0x80 ]))

  // U+DBFF U+DFFF
  expectTrue(checkDecodeUTF8(
      [],
      [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
      [ 0xed, 0xaf, 0xbf, 0xed, 0xbf, 0xbf ]))
}

UTF8Decoder.test("Noncharacters") {
  //
  // Noncharacters
  //

  // Unicode 6.3.0:
  //
  //    D14.  Noncharacter: A code point that is permanently reserved for
  //    internal use and that should never be interchanged. Noncharacters
  //    consist of the values U+nFFFE and U+nFFFF (where n is from 0 to 1016)
  //    and the values U+FDD0..U+FDEF.

  // U+FFFE
  expectTrue(checkDecodeUTF8([ 0xfffe ], [], [ 0xef, 0xbf, 0xbe ]))

  // U+FFFF
  expectTrue(checkDecodeUTF8([ 0xffff ], [], [ 0xef, 0xbf, 0xbf ]))

  // U+1FFFE
  expectTrue(checkDecodeUTF8([ 0x1fffe ], [], [ 0xf0, 0x9f, 0xbf, 0xbe ]))

  // U+1FFFF
  expectTrue(checkDecodeUTF8([ 0x1ffff ], [], [ 0xf0, 0x9f, 0xbf, 0xbf ]))

  // U+2FFFE
  expectTrue(checkDecodeUTF8([ 0x2fffe ], [], [ 0xf0, 0xaf, 0xbf, 0xbe ]))

  // U+2FFFF
  expectTrue(checkDecodeUTF8([ 0x2ffff ], [], [ 0xf0, 0xaf, 0xbf, 0xbf ]))

  // U+3FFFE
  expectTrue(checkDecodeUTF8([ 0x3fffe ], [], [ 0xf0, 0xbf, 0xbf, 0xbe ]))

  // U+3FFFF
  expectTrue(checkDecodeUTF8([ 0x3ffff ], [], [ 0xf0, 0xbf, 0xbf, 0xbf ]))

  // U+4FFFE
  expectTrue(checkDecodeUTF8([ 0x4fffe ], [], [ 0xf1, 0x8f, 0xbf, 0xbe ]))

  // U+4FFFF
  expectTrue(checkDecodeUTF8([ 0x4ffff ], [], [ 0xf1, 0x8f, 0xbf, 0xbf ]))

  // U+5FFFE
  expectTrue(checkDecodeUTF8([ 0x5fffe ], [], [ 0xf1, 0x9f, 0xbf, 0xbe ]))

  // U+5FFFF
  expectTrue(checkDecodeUTF8([ 0x5ffff ], [], [ 0xf1, 0x9f, 0xbf, 0xbf ]))

  // U+6FFFE
  expectTrue(checkDecodeUTF8([ 0x6fffe ], [], [ 0xf1, 0xaf, 0xbf, 0xbe ]))

  // U+6FFFF
  expectTrue(checkDecodeUTF8([ 0x6ffff ], [], [ 0xf1, 0xaf, 0xbf, 0xbf ]))

  // U+7FFFE
  expectTrue(checkDecodeUTF8([ 0x7fffe ], [], [ 0xf1, 0xbf, 0xbf, 0xbe ]))

  // U+7FFFF
  expectTrue(checkDecodeUTF8([ 0x7ffff ], [], [ 0xf1, 0xbf, 0xbf, 0xbf ]))

  // U+8FFFE
  expectTrue(checkDecodeUTF8([ 0x8fffe ], [], [ 0xf2, 0x8f, 0xbf, 0xbe ]))

  // U+8FFFF
  expectTrue(checkDecodeUTF8([ 0x8ffff ], [], [ 0xf2, 0x8f, 0xbf, 0xbf ]))

  // U+9FFFE
  expectTrue(checkDecodeUTF8([ 0x9fffe ], [], [ 0xf2, 0x9f, 0xbf, 0xbe ]))

  // U+9FFFF
  expectTrue(checkDecodeUTF8([ 0x9ffff ], [], [ 0xf2, 0x9f, 0xbf, 0xbf ]))

  // U+AFFFE
  expectTrue(checkDecodeUTF8([ 0xafffe ], [], [ 0xf2, 0xaf, 0xbf, 0xbe ]))

  // U+AFFFF
  expectTrue(checkDecodeUTF8([ 0xaffff ], [], [ 0xf2, 0xaf, 0xbf, 0xbf ]))

  // U+BFFFE
  expectTrue(checkDecodeUTF8([ 0xbfffe ], [], [ 0xf2, 0xbf, 0xbf, 0xbe ]))

  // U+BFFFF
  expectTrue(checkDecodeUTF8([ 0xbffff ], [], [ 0xf2, 0xbf, 0xbf, 0xbf ]))

  // U+CFFFE
  expectTrue(checkDecodeUTF8([ 0xcfffe ], [], [ 0xf3, 0x8f, 0xbf, 0xbe ]))

  // U+CFFFF
  expectTrue(checkDecodeUTF8([ 0xcfffF ], [], [ 0xf3, 0x8f, 0xbf, 0xbf ]))

  // U+DFFFE
  expectTrue(checkDecodeUTF8([ 0xdfffe ], [], [ 0xf3, 0x9f, 0xbf, 0xbe ]))

  // U+DFFFF
  expectTrue(checkDecodeUTF8([ 0xdffff ], [], [ 0xf3, 0x9f, 0xbf, 0xbf ]))

  // U+EFFFE
  expectTrue(checkDecodeUTF8([ 0xefffe ], [], [ 0xf3, 0xaf, 0xbf, 0xbe ]))

  // U+EFFFF
  expectTrue(checkDecodeUTF8([ 0xeffff ], [], [ 0xf3, 0xaf, 0xbf, 0xbf ]))

  // U+FFFFE
  expectTrue(checkDecodeUTF8([ 0xffffe ], [], [ 0xf3, 0xbf, 0xbf, 0xbe ]))

  // U+FFFFF
  expectTrue(checkDecodeUTF8([ 0xfffff ], [], [ 0xf3, 0xbf, 0xbf, 0xbf ]))

  // U+10FFFE
  expectTrue(checkDecodeUTF8([ 0x10fffe ], [], [ 0xf4, 0x8f, 0xbf, 0xbe ]))

  // U+10FFFF
  expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

  // U+FDD0
  expectTrue(checkDecodeUTF8([ 0xfdd0 ], [], [ 0xef, 0xb7, 0x90 ]))

  // U+FDD1
  expectTrue(checkDecodeUTF8([ 0xfdd1 ], [], [ 0xef, 0xb7, 0x91 ]))

  // U+FDD2
  expectTrue(checkDecodeUTF8([ 0xfdd2 ], [], [ 0xef, 0xb7, 0x92 ]))

  // U+FDD3
  expectTrue(checkDecodeUTF8([ 0xfdd3 ], [], [ 0xef, 0xb7, 0x93 ]))

  // U+FDD4
  expectTrue(checkDecodeUTF8([ 0xfdd4 ], [], [ 0xef, 0xb7, 0x94 ]))

  // U+FDD5
  expectTrue(checkDecodeUTF8([ 0xfdd5 ], [], [ 0xef, 0xb7, 0x95 ]))

  // U+FDD6
  expectTrue(checkDecodeUTF8([ 0xfdd6 ], [], [ 0xef, 0xb7, 0x96 ]))

  // U+FDD7
  expectTrue(checkDecodeUTF8([ 0xfdd7 ], [], [ 0xef, 0xb7, 0x97 ]))

  // U+FDD8
  expectTrue(checkDecodeUTF8([ 0xfdd8 ], [], [ 0xef, 0xb7, 0x98 ]))

  // U+FDD9
  expectTrue(checkDecodeUTF8([ 0xfdd9 ], [], [ 0xef, 0xb7, 0x99 ]))

  // U+FDDA
  expectTrue(checkDecodeUTF8([ 0xfdda ], [], [ 0xef, 0xb7, 0x9a ]))

  // U+FDDB
  expectTrue(checkDecodeUTF8([ 0xfddb ], [], [ 0xef, 0xb7, 0x9b ]))

  // U+FDDC
  expectTrue(checkDecodeUTF8([ 0xfddc ], [], [ 0xef, 0xb7, 0x9c ]))

  // U+FDDD
  expectTrue(checkDecodeUTF8([ 0xfddd ], [], [ 0xef, 0xb7, 0x9d ]))

  // U+FDDE
  expectTrue(checkDecodeUTF8([ 0xfdde ], [], [ 0xef, 0xb7, 0x9e ]))

  // U+FDDF
  expectTrue(checkDecodeUTF8([ 0xfddf ], [], [ 0xef, 0xb7, 0x9f ]))

  // U+FDE0
  expectTrue(checkDecodeUTF8([ 0xfde0 ], [], [ 0xef, 0xb7, 0xa0 ]))

  // U+FDE1
  expectTrue(checkDecodeUTF8([ 0xfde1 ], [], [ 0xef, 0xb7, 0xa1 ]))

  // U+FDE2
  expectTrue(checkDecodeUTF8([ 0xfde2 ], [], [ 0xef, 0xb7, 0xa2 ]))

  // U+FDE3
  expectTrue(checkDecodeUTF8([ 0xfde3 ], [], [ 0xef, 0xb7, 0xa3 ]))

  // U+FDE4
  expectTrue(checkDecodeUTF8([ 0xfde4 ], [], [ 0xef, 0xb7, 0xa4 ]))

  // U+FDE5
  expectTrue(checkDecodeUTF8([ 0xfde5 ], [], [ 0xef, 0xb7, 0xa5 ]))

  // U+FDE6
  expectTrue(checkDecodeUTF8([ 0xfde6 ], [], [ 0xef, 0xb7, 0xa6 ]))

  // U+FDE7
  expectTrue(checkDecodeUTF8([ 0xfde7 ], [], [ 0xef, 0xb7, 0xa7 ]))

  // U+FDE8
  expectTrue(checkDecodeUTF8([ 0xfde8 ], [], [ 0xef, 0xb7, 0xa8 ]))

  // U+FDE9
  expectTrue(checkDecodeUTF8([ 0xfde9 ], [], [ 0xef, 0xb7, 0xa9 ]))

  // U+FDEA
  expectTrue(checkDecodeUTF8([ 0xfdea ], [], [ 0xef, 0xb7, 0xaa ]))

  // U+FDEB
  expectTrue(checkDecodeUTF8([ 0xfdeb ], [], [ 0xef, 0xb7, 0xab ]))

  // U+FDEC
  expectTrue(checkDecodeUTF8([ 0xfdec ], [], [ 0xef, 0xb7, 0xac ]))

  // U+FDED
  expectTrue(checkDecodeUTF8([ 0xfded ], [], [ 0xef, 0xb7, 0xad ]))

  // U+FDEE
  expectTrue(checkDecodeUTF8([ 0xfdee ], [], [ 0xef, 0xb7, 0xae ]))

  // U+FDEF
  expectTrue(checkDecodeUTF8([ 0xfdef ], [], [ 0xef, 0xb7, 0xaf ]))

  // U+FDF0
  expectTrue(checkDecodeUTF8([ 0xfdf0 ], [], [ 0xef, 0xb7, 0xb0 ]))

  // U+FDF1
  expectTrue(checkDecodeUTF8([ 0xfdf1 ], [], [ 0xef, 0xb7, 0xb1 ]))

  // U+FDF2
  expectTrue(checkDecodeUTF8([ 0xfdf2 ], [], [ 0xef, 0xb7, 0xb2 ]))

  // U+FDF3
  expectTrue(checkDecodeUTF8([ 0xfdf3 ], [], [ 0xef, 0xb7, 0xb3 ]))

  // U+FDF4
  expectTrue(checkDecodeUTF8([ 0xfdf4 ], [], [ 0xef, 0xb7, 0xb4 ]))

  // U+FDF5
  expectTrue(checkDecodeUTF8([ 0xfdf5 ], [], [ 0xef, 0xb7, 0xb5 ]))

  // U+FDF6
  expectTrue(checkDecodeUTF8([ 0xfdf6 ], [], [ 0xef, 0xb7, 0xb6 ]))

  // U+FDF7
  expectTrue(checkDecodeUTF8([ 0xfdf7 ], [], [ 0xef, 0xb7, 0xb7 ]))

  // U+FDF8
  expectTrue(checkDecodeUTF8([ 0xfdf8 ], [], [ 0xef, 0xb7, 0xb8 ]))

  // U+FDF9
  expectTrue(checkDecodeUTF8([ 0xfdf9 ], [], [ 0xef, 0xb7, 0xb9 ]))

  // U+FDFA
  expectTrue(checkDecodeUTF8([ 0xfdfa ], [], [ 0xef, 0xb7, 0xba ]))

  // U+FDFB
  expectTrue(checkDecodeUTF8([ 0xfdfb ], [], [ 0xef, 0xb7, 0xbb ]))

  // U+FDFC
  expectTrue(checkDecodeUTF8([ 0xfdfc ], [], [ 0xef, 0xb7, 0xbc ]))

  // U+FDFD
  expectTrue(checkDecodeUTF8([ 0xfdfd ], [], [ 0xef, 0xb7, 0xbd ]))

  // U+FDFE
  expectTrue(checkDecodeUTF8([ 0xfdfe ], [], [ 0xef, 0xb7, 0xbe ]))

  // U+FDFF
  expectTrue(checkDecodeUTF8([ 0xfdff ], [], [ 0xef, 0xb7, 0xbf ]))
}

UTF8Decoder.run()
// CHECK: {{^}}UTF8Decoder: All tests passed

