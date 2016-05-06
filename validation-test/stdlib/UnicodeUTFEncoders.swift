// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -O
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: objc_interop

import SwiftPrivate
import StdlibUnittest


import Foundation

protocol TestableUnicodeCodec : UnicodeCodec {
  associatedtype CodeUnit : Integer
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
var unicodeScalarRanges: [CountableClosedRange<UInt32>] = [UInt32(0)...0xd7ff, 0xe000...0x10ffff]

var unicodeScalarCount: Int {
  var count = 0
  for r in unicodeScalarRanges {
    count += Int(r.upperBound - r.lowerBound)
  }
  return count
}

func nthUnicodeScalar(_ n: UInt32) -> UnicodeScalar {
  var count: UInt32 = 0
  for r in unicodeScalarRanges {
    count += r.upperBound - r.lowerBound
    if count > n {
      return UnicodeScalar(r.upperBound - (count - n))
    }
  }
  _preconditionFailure("Index out of range")
}

// `buffer` should have a length >= 4
func nsEncode<CodeUnit>(
  _ c: UInt32,
  _ encoding: NSStringEncoding,
  _ buffer: inout [CodeUnit],
  _ used: inout Int
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
    remaining: nil)
}

final class CodecTest<Codec : TestableUnicodeCodec> {
  var used = 0
  typealias CodeUnit = Codec.CodeUnit
  var nsEncodeBuffer: [CodeUnit] = Array(repeating: 0, count: 4)
  var encodeBuffer: [CodeUnit] = Array(repeating: 0, count: 4)

  final func testOne(_ scalar: UnicodeScalar) {
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
      self.encodeBuffer[encodeIndex] = $0
      encodeIndex += 1
    }

    var iter = nsEncoded.makeIterator()
    var decoded: UnicodeScalar
    var decoder = Codec()
    switch decoder.decode(&iter) {
    case .scalarValue(let us):
      decoded = us
    default:
      fatalError("decoding failed")
    }
    expectEqual(
      scalar, decoded,
      "Decoding failed: \(asHex(scalar.value)) => " +
      "\(asHex(nsEncoded)) => \(asHex(decoded.value))"
    ) { $0 == $1 }

    encodeIndex = encodeBuffer.startIndex
    Codec.encode(scalar, sendingOutputTo: encodeOutput)
    expectEqual(
      nsEncoded, encodeBuffer[0..<encodeIndex],
      "Decoding failed: \(asHex(nsEncoded)) => " +
        "\(asHex(scalar.value)) => \(asHex(self.encodeBuffer[0]))"
    ) { $0 == $1 }
  }

  final func run(_ minScalarOrd: Int, _ maxScalarOrd: Int) {
    print("testing \(Codec.name())")
    for i in minScalarOrd..<maxScalarOrd {
      testOne(nthUnicodeScalar(UInt32(i)))
    }
  }
}

var UTFEncoders = TestSuite("UTFEncoders")

UTFEncoders.test("encode") {
  let minScalarOrd = 0
  let maxScalarOrd = unicodeScalarCount
  CodecTest<UTF8>().run(minScalarOrd, maxScalarOrd)
  CodecTest<UTF16>().run(minScalarOrd, maxScalarOrd)
  CodecTest<UTF32>().run(minScalarOrd, maxScalarOrd)
}

runAllTests()

