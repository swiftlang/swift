// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import SwiftPrivate
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

import Foundation

@_silgen_name("random") func random() -> UInt32
@_silgen_name("srandomdev") func srandomdev()

protocol TestableUnicodeCodec : UnicodeCodecType {
  typealias CodeUnit : IntegerType
  static func encodingId() -> NSStringEncoding
  static func name() -> NSString
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

// The valid ranges of Unicode scalar values
var unicodeScalarRanges: [Range<UInt32>] = [UInt32(0)...0xd7ff, 0xe000...0x10ffff]

var unicodeScalarCount: Int {
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

// `buffer` should have a length >= 4
func nsEncode<CodeUnit>(
  c: UInt32,
  _ encoding: NSStringEncoding,
  inout _ buffer: [CodeUnit],
  inout _ used: Int
) {
  var c = c
  _precondition(buffer.count >= 4, "buffer is not large enough")

  let s = NSString(
    bytes: &c,
    length: 4,
    encoding: NSUTF32LittleEndianStringEncoding)!

  s.getBytes(
    &buffer,
    maxLength: buffer.count,
    usedLength: &used,
    encoding: encoding,
    options: [],
    range: NSRange(location: 0, length: s.length),
    remainingRange: nil)
}

class CodecTest<Codec : TestableUnicodeCodec> {
  var used = 0
  typealias CodeUnit = Codec.CodeUnit
  var nsEncodeBuffer: [CodeUnit] = Array(count: 4, repeatedValue: 0)
  var encodeBuffer: [CodeUnit] = Array(count: 4, repeatedValue: 0)

  func testOne(scalar: UnicodeScalar) {
    /* Progress reporter
    if (scalar.value % 0x1000) == 0 {
      print("\(asHex(scalar.value))")
    }
    */

    // Use Cocoa to encode the scalar
    nsEncode(scalar.value, Codec.encodingId(), &nsEncodeBuffer, &used)
    let nsEncoded = nsEncodeBuffer[0..<(used/sizeof(CodeUnit.self))]
    var encodeIndex = encodeBuffer.startIndex
    let encodeOutput: (CodeUnit) -> Void = {
      self.encodeBuffer[encodeIndex++] = $0
    }

    var g = nsEncoded.generate()
    var decoded: UnicodeScalar
    var decoder = Codec()
    switch decoder.decode(&g) {
    case .Result(let us):
      decoded = us
    default:
      fatalError("decoding failed")
    }
    expectEqual(
      scalar, decoded,
      "Decoding failed: \(asHex(scalar.value)) => " +
      "\(asHex(nsEncoded)) => \(asHex(decoded.value))"
    )

    encodeIndex = encodeBuffer.startIndex
    Codec.encode(scalar, output: encodeOutput)
    expectEqual(
      nsEncoded, encodeBuffer[0..<encodeIndex],
      "Decoding failed: \(asHex(nsEncoded)) => " +
        "\(asHex(scalar.value)) => \(asHex(self.encodeBuffer[0]))"
    )
  }

  func run(minScalarOrd: Int, _ maxScalarOrd: Int) {
    print("testing \(Codec.name())")
    for i in minScalarOrd..<maxScalarOrd {
      testOne(nthUnicodeScalar(UInt32(i)))
    }
  }
}

var UTFEncoders = TestSuite("UTFEncoders")

UTFEncoders.test("encodeRandomBlock") {
  srandomdev()
  // To avoid swamping the buildbot, by default, test only one out of
  // testGroupCount cases, selected at random.  You can adjust the `testAll`
  // variable below to test everything.
  var testGroupCount = 128
  var testGroup = random() % testGroupCount
  var testAll = false
  var minScalarOrd: Int
  var maxScalarOrd: Int

  if testAll {
    print("Testing all Unicode scalars")
    minScalarOrd = 0
    maxScalarOrd = unicodeScalarCount
  } else {
    print("Testing Unicode scalar group \(testGroup) of \(testGroupCount)")
    minScalarOrd = unicodeScalarCount * testGroup / testGroupCount
    maxScalarOrd = unicodeScalarCount * (testGroup+1) / testGroupCount
  }

  CodecTest<UTF8>().run(minScalarOrd, maxScalarOrd)
  CodecTest<UTF16>().run(minScalarOrd, maxScalarOrd)
  CodecTest<UTF32>().run(minScalarOrd, maxScalarOrd)
}

runAllTests()

