// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: optimized_stdlib

// rdar://124539686
// UNSUPPORTED: CPU=arm64e, CPU=arm64

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

func check(
  _ string: String,
  _ pieces: [[Unicode.Scalar]],
  file: String = #file, line: UInt = #line
) {
  expectEqual(
    string.forwardPieces, pieces,
    "string: \(String(reflecting: string)) (forward)",
    file: file, line: line)
  expectEqual(
    string.backwardPieces, pieces,
    "string: \(String(reflecting: string)) (backward)",
    file: file, line: line)
}

if #available(SwiftStdlib 5.9, *) {
  StringGraphemeBreaking.test("grapheme breaking") {
    for test in graphemeBreakTests {
      check(test.string, test.pieces)
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

  init(_ value: some Sequence<UInt16>) {
    _value = Array(value)
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

if #available(SwiftStdlib 5.9, *) {
  StringGraphemeBreaking.test("grapheme breaking foreign") {
    for test in graphemeBreakTests {
      let foreign = NonContiguousNSString(test.string.utf16)
      let string = foreign as String

      expectTrue(string._guts._isForeign())
      check(string, test.pieces)
    }
  }

  StringGraphemeBreaking.test("GB11") {
    // MAN, ZERO WIDTH JOINER, ZERO WIDTH JOINER, GIRL
    let string = "\u{1f468}\u{200d}\u{200d}\u{1f467}"
    let pieces: [[Unicode.Scalar]] = [
      ["\u{1f468}", "\u{200d}", "\u{200d}"],
      ["\u{1f467}"]
    ]
    check(string, pieces)

    let foreign = NonContiguousNSString(string.utf16) as String
    expectTrue(foreign._guts._isForeign())
    check(foreign, pieces)
  }
}
