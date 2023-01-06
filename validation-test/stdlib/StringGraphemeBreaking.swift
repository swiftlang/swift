// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: optimized_stdlib

import StdlibUnittest
import StdlibUnicodeUnittest
import Foundation

let StringGraphemeBreaking = TestSuite("StringGraphemeBreaking")
defer { runAllTests() }

extension String {
  var forwardPieces: [[Unicode.Scalar]] {
    var i = startIndex
    var r: [[Unicode.Scalar]] = []
    while i < endIndex {
      let j = self.index(after: i)
      r.append(Array(self[i..<j].unicodeScalars))
      i = j
    }
    return r
  }

  var backwardPieces: [[Unicode.Scalar]] {
    var j = endIndex
    var r: [[Unicode.Scalar]] = []
    while j > startIndex {
      let i = self.index(before: j)
      r.append(Array(self[i..<j].unicodeScalars))
      j = i
    }
    r.reverse()
    return r
  }
}

if #available(SwiftStdlib 5.6, *) {
  StringGraphemeBreaking.test("grapheme breaking") {
    for test in graphemeBreakTests {
      expectEqual(
        test.string.forwardPieces, test.pieces,
        "string: \(String(reflecting: test.string)) (forward)")
      expectEqual(
        test.string.backwardPieces, test.pieces,
        "string: \(String(reflecting: test.string)) (backward)")
    }
  }
}

// The most simple subclass of NSString that CoreFoundation does not know
// about.
class NonContiguousNSString: NSString {
  required init(coder aDecoder: NSCoder) {
    fatalError("don't call this initializer")
  }
  required init(itemProviderData data: Data, typeIdentifier: String) throws {
    fatalError("don't call this initializer")
  }

  override init() {
    _value = []
    super.init()
  }

  init(_ value: [UInt16]) {
    _value = value
    super.init()
  }

  @objc(copyWithZone:) override func copy(with zone: NSZone?) -> Any {
    // Ensure that copying this string produces a class that CoreFoundation
    // does not know about.
    return self
  }

  @objc override var length: Int {
    return _value.count
  }

  @objc override func character(at index: Int) -> unichar {
    return _value[index]
  }

  var _value: [UInt16]
}

extension _StringGuts {
  @_silgen_name("$ss11_StringGutsV9isForeignSbvg")
  func _isForeign() -> Bool
}

if #available(SwiftStdlib 5.6, *) {
  StringGraphemeBreaking.test("grapheme breaking foreign") {
    for test in graphemeBreakTests {
      let foreign = NonContiguousNSString(Array(test.string.utf16))
      let string = foreign as String

      expectTrue(string._guts._isForeign())
      expectEqual(
        string.forwardPieces, test.pieces,
        "string: \(String(reflecting: test.string)) (forward)")
      expectEqual(
        string.backwardPieces, test.pieces,
        "string: \(String(reflecting: test.string)) (backward)")
    }
  }
}
