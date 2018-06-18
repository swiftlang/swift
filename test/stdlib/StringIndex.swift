// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var StringIndexTests = TestSuite("StringIndexTests")

enum SimpleString: String {
  case smallASCII = "abcdefg"
  case smallUnicode = "abéÏ𓀀"
  case largeASCII = "012345678901234567890"
  case largeUnicode = "abéÏ012345678901234567890𓀀"
  case emoji = "😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦"
}

let simpleStrings: [String] = [
    SimpleString.smallASCII.rawValue,
    SimpleString.smallUnicode.rawValue,
    SimpleString.largeASCII.rawValue,
    SimpleString.largeUnicode.rawValue,
    SimpleString.emoji.rawValue,
]

StringIndexTests.test("basic sanity checks") {
  for s in simpleStrings {
    let utf8 = Array(s.utf8)
    let subUTF8 = Array(s[...].utf8)
    let utf16 = Array(s.utf16)
    let subUTF16 = Array(s[...].utf16)
    let utf32 = Array(s.unicodeScalars.map { $0.value })
    let subUTF32 = Array(s[...].unicodeScalars.map { $0.value })

    expectEqual(s, String(decoding: utf8, as: UTF8.self))
    expectEqual(s, String(decoding: subUTF8, as: UTF8.self))
    expectEqual(s, String(decoding: utf16, as: UTF16.self))
    expectEqual(s, String(decoding: subUTF16, as: UTF16.self))
    expectEqual(s, String(decoding: utf32, as: UTF32.self))
    expectEqual(s, String(decoding: subUTF32, as: UTF32.self))
  }
}

StringIndexTests.test("view counts") {
  func validateViewCount<View: BidirectionalCollection>(
    _ view: View, for string: String,
    stackTrace: SourceLocStack = SourceLocStack(),
    showFrame: Bool = true,
    file: String = #file, line: UInt = #line
  ) where View.Element: Equatable, View.Index == String.Index {

    var stackTrace = stackTrace.pushIf(showFrame, file: file, line: line)

    let count = view.count
    func expect(_ i: Int,
      file: String = #file, line: UInt = #line
    ) {
      expectEqual(count, i, "for String: \(string)",
        stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
        showFrame: false)
    }

    let reversedView = view.reversed()

    expect(Array(view).count)
    expect(view.indices.count)
    expect(view.indices.reversed().count)
    expect(reversedView.indices.count)
    expect(view.distance(from: view.startIndex, to: view.endIndex))
    expect(reversedView.distance(
      from: reversedView.startIndex, to: reversedView.endIndex))

    // Access the elements from the indices
    expectEqual(Array(view), view.indices.map { view[$0] })
    expectEqual(
      Array(reversedView), reversedView.indices.map { reversedView[$0] })

    let indicesArray = Array<String.Index>(view.indices)
    for i in 0..<indicesArray.count {
      var idx = view.startIndex
      idx = view.index(idx, offsetBy: i)
      expectEqual(indicesArray[i], idx)
    }
  }

  for s in simpleStrings {
    validateViewCount(s, for: s)
    validateViewCount(s.utf8, for: s)
    validateViewCount(s.utf16, for: s)
    validateViewCount(s.unicodeScalars, for: s)

    validateViewCount(s[...], for: s)
    validateViewCount(s[...].utf8, for: s)
    validateViewCount(s[...].utf16, for: s)
    validateViewCount(s[...].unicodeScalars, for: s)
  }
}

StringIndexTests.test("interchange") {
  // Basic index alignment

  func validateIndices(_ s: String) {
    let utf8Indices = s.utf8.indices
    let utf16Indices = s.utf16.indices
    let unicodeScalarIndices = s.unicodeScalars.indices
    let characterIndices = s.indices

    for idx in utf8Indices {
      let char = s.utf8[idx]

      // ASCII or leading code unit in the scalar
      if char <= 0x7F || char >= 0b1100_0000 {
        expectEqual(idx, idx.samePosition(in: s.unicodeScalars))
        expectEqual(idx, idx.samePosition(in: s.utf16))

        // ASCII
        if char <= 0x7F {
          expectEqual(UInt16(char), s.utf16[idx])
          expectEqual(UInt32(char), s.unicodeScalars[idx].value)
        }
      } else {
        // Continuation code unit
        assert(char & 0b1100_0000 == 0b1000_0000)
        expectNil(idx.samePosition(in: s))
        expectNil(idx.samePosition(in: s.utf16))
        expectNil(idx.samePosition(in: s.unicodeScalars))
      }
    }
  }

  for s in simpleStrings {
    validateIndices(s)
  }
}

runAllTests()