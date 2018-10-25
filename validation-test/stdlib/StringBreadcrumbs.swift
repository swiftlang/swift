// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// Some targetted tests for the breadcrumbs path. There is some overlap with
// UTF16View tests for huge strings, but we want a simpler suite that targets
// some corner cases specifically.

import Swift
import StdlibUnittest

let smallASCII = "abcdefg"
let smallUnicode = "abéÏ𓀀"
let largeASCII = "012345678901234567890"
let largeUnicode = "abéÏ012345678901234567890𓀀"
let emoji = "😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦"
let chinese = "Swift 是面向 Apple 平台的编程语言，功能强大且直观易用，而本次更新对其进行了全面优化。"

let largeString: String = {
  var result = ""
  result += smallASCII
  result += smallUnicode
  result += largeASCII
  result += chinese
  result += largeUnicode
  result += emoji
  result += smallASCII
  result += result.reversed()
  return result
}()

let StringBreadcrumbsTests = TestSuite("StringBreadcrumbsTests")

StringBreadcrumbsTests.test("largeString") {
  var utf16CodeUnits = Array(largeString.utf16)
  var utf16Indices = Array(largeString.utf16.indices)
  var outputBuffer = Array<UInt16>(repeating: 0, count: utf16CodeUnits.count)

  for i in 0..<(utf16CodeUnits.count-1) {
    for j in (i+1)..<utf16CodeUnits.count {
      let range = Range(uncheckedBounds: (i, j))
      let indexRange = largeString._toUTF16Indices(range)

      // Range<String.Index> from Range<Int>
      expectEqualSequence(
        utf16CodeUnits[i..<j], largeString.utf16[indexRange])

      // Copy characters      
      outputBuffer.withUnsafeMutableBufferPointer {
        largeString._copyUTF16CodeUnits(into: $0, range: range)
      }
      expectEqualSequence(utf16CodeUnits[i..<j], outputBuffer[..<range.count])

      // Range<Int> from Range<String.Index>
      let roundTripOffsets = largeString._toUTF16Offsets(indexRange)
      expectEqualSequence(range, roundTripOffsets)

      // Single Int <=> String.Index
      expectEqual(indexRange.lowerBound, largeString._toUTF16Index(i))
      expectEqual(indexRange.upperBound, largeString._toUTF16Index(j))
      expectEqual(i, largeString._toUTF16Offset(indexRange.lowerBound))
      expectEqual(j, largeString._toUTF16Offset(indexRange.upperBound))
    }
  }
}

// TODO(String testing): hammer breadcrumb boundaries more, maybe internals too

runAllTests()

