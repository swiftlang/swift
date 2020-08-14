// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test
// REQUIRES: foundation

// With a non-optimized stdlib the test takes very long.
// REQUIRES: optimized_stdlib

import SwiftPrivate
import StdlibUnittest
import Foundation

protocol TestableUnicodeCodec: UnicodeCodec {

  static var nsEncoding: String.Encoding { get }
}

extension UTF8: TestableUnicodeCodec {

  static var nsEncoding: String.Encoding { .utf8 }
}

extension UTF16: TestableUnicodeCodec {

  static var nsEncoding: String.Encoding {
    #if _endian(little)
    return .utf16LittleEndian
    #else
    return .utf16BigEndian
    #endif
  }
}

extension UTF32: TestableUnicodeCodec {

  static var nsEncoding: String.Encoding {
    #if _endian(little)
    return .utf32LittleEndian
    #else
    return .utf32BigEndian
    #endif
  }
}

//===----------------------------------------------------------------------===//

final class CodecTest<Codec: TestableUnicodeCodec> {

  typealias CodeUnitBuffer = UnsafeMutableBufferPointer<Codec.CodeUnit>

  var _unicodeScalar: Unicode.Scalar = "\0"
  var _nsEncodedScalar: Slice<CodeUnitBuffer>
  var _encodedScalar: Slice<CodeUnitBuffer>

  init() {
    _nsEncodedScalar = CodeUnitBuffer.allocate(capacity: 4)[...]
    _encodedScalar = CodeUnitBuffer.allocate(capacity: 4)[...]
  }

  deinit {
    _nsEncodedScalar.base.deallocate()
    _encodedScalar.base.deallocate()
  }

  func testNSString() -> Bool {
    // Test the `NSString.init(bytes:length:encoding:)` API.
    guard let nsString = withUnsafeBytes(of: _unicodeScalar.value, {
      NSString(
        bytes: $0.baseAddress!,
        length: $0.count,
        encoding: UTF32.nsEncoding.rawValue
      )
    }) else {
      expectUnreachable(
        """
        `NSString.init(bytes:length:encoding:)` failed: \
        \(asHex(_unicodeScalar.value)) => ???
        """
      )
      return false
    }

    // Test the `NSString.getBytes` API.
    var usedLength = 0
    let codeUnitStride = MemoryLayout<Codec.CodeUnit>.stride
    let hasBytes = nsString.getBytes(
      _nsEncodedScalar.base.baseAddress,
      maxLength: _nsEncodedScalar.base.count,
      usedLength: &usedLength,
      encoding: Codec.nsEncoding.rawValue,
      options: [],
      range: NSRange(location: 0, length: nsString.length),
      remaining: nil
    )
    guard
      hasBytes,
      (1...4).contains(usedLength),
      usedLength.isMultiple(of: codeUnitStride)
    else {
      expectUnreachable(
        """
        `NSString.getBytes(_:maxLength:usedLength:\
        encoding:options:range:remaining:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar.base)), \
        usedLength = \(usedLength)
        """
      )
      return false
    }

    _nsEncodedScalar = _nsEncodedScalar.base.prefix(usedLength / codeUnitStride)
    return true
  }

  func testUnicodeCodec() {
    // Test the `UnicodeCodec.encode(_:into:)` API.
    _encodedScalar = _encodedScalar.base[...]
    var encodedScalarIndex = _encodedScalar.startIndex
    Codec.encode(_unicodeScalar, into: { [self] in
      _encodedScalar[encodedScalarIndex] = $0
      _encodedScalar.formIndex(after: &encodedScalarIndex)
    })
    _encodedScalar = _encodedScalar.base[..<encodedScalarIndex]
    expectEqualSequence(
      _nsEncodedScalar, _encodedScalar,
      """
      `UnicodeCodec.encode(_:into:)` failed: \
      \(asHex(_unicodeScalar.value)) => \
      \(asHex(_nsEncodedScalar)) => \
      \(asHex(_encodedScalar))
      """
    )

    // Test the `UnicodeCodec.decode(_:)` API.
    var iterator = _nsEncodedScalar.makeIterator()
    var decoder = Codec()
    switch decoder.decode(&iterator) {
    case .scalarValue(let decodedScalar):
      expectEqual(
        _unicodeScalar, decodedScalar,
        """
        `UnicodeCodec.decode(_:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar)) => \
        \(asHex(decodedScalar.value))
        """
      )
    default:
      expectUnreachable(
        """
        `UnicodeCodec.decode(_:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar)) => ???
        """
      )
    }
  }

  func testUnicodeEncoding() {
    // Test the `Unicode.Encoding.encode(_:)` API.
    guard let encodedScalar = Codec.encode(_unicodeScalar) else {
      expectUnreachable(
        """
        `Unicode.Encoding.encode(_:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar)) => ???
        """
      )
      return
    }
    expectEqualSequence(
      _nsEncodedScalar, encodedScalar,
      """
      `Unicode.Encoding.encode(_:)` failed: \
      \(asHex(_unicodeScalar.value)) => \
      \(asHex(_nsEncodedScalar)) => \
      \(asHex(encodedScalar))
      """
    )

    // Test the `Unicode.Encoding.decode(_:)` API.
    let decodedScalar = Codec.decode(encodedScalar)
    expectEqual(
      _unicodeScalar, decodedScalar,
      """
      `Unicode.Encoding.decode(_:)` failed: \
      \(asHex(_unicodeScalar.value)) => \
      \(asHex(encodedScalar)) => \
      \(asHex(decodedScalar.value))
      """
    )
  }

  func testUnicodeParser() {
    // Test the `Unicode.Parser.parseScalar(from:)` API.
    var iterator = _nsEncodedScalar.makeIterator()
    var parser = Codec.ForwardParser()
    switch parser.parseScalar(from: &iterator) {
    case .valid(let encodedScalar):
      expectEqualSequence(
        _nsEncodedScalar, encodedScalar,
        """
        `Unicode.Parser.parseScalar(from:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar)) => \
        \(asHex(encodedScalar))
        """
      )
    default:
      expectUnreachable(
        """
        `Unicode.Parser.parseScalar(from:)` failed: \
        \(asHex(_unicodeScalar.value)) => \
        \(asHex(_nsEncodedScalar)) => ???
        """
      )
    }
  }

  func run() {
    for value: UInt32 in 0...0x10FFFF {
      if let unicodeScalar = Unicode.Scalar(value) {
        _unicodeScalar = unicodeScalar
        _nsEncodedScalar.base.initialize(repeating: 0)
        _encodedScalar.base.initialize(repeating: 0)

        if testNSString() {
          testUnicodeCodec()
          testUnicodeEncoding()
          testUnicodeParser()
        }
      }
    }
  }
}

//===----------------------------------------------------------------------===//

var CodecTestSuite = TestSuite("CodecTestSuite")

CodecTestSuite.test("UTF8") {
  CodecTest<UTF8>().run()
}

CodecTestSuite.test("UTF16") {
  CodecTest<UTF16>().run()
}

CodecTestSuite.test("UTF32") {
  CodecTest<UTF32>().run()
}

runAllTests()
