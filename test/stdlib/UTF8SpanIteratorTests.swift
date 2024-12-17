// RUN: %target-run-stdlib-swift(-enable-experimental-feature Span) -enable-experimental-feature Span %S/Inputs/

// REQUIRES: executable_test

import Swift
import StdlibUnittest

var suite = TestSuite("UTF8SpanIterator")
defer { runAllTests() }

@available(SwiftStdlib 6.1, *)
extension Array {
  func withSpan<R>(_ f: (Span<Element>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
}

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  func withSpan<R>(_ f: (Span<UInt8>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
}

// 
@available(SwiftStdlib 6.1, *)
struct ContentEquivalenceTestCase {
  var str: String
  var loc: SourceLocStack

  func withUTF8Span<R>(_ f: (UTF8Span) throws -> R) rethrows -> R {
    try Array(str.utf8).withSpan { span in 
      try f(try! UTF8Span(_validating: span))
    }
  }

  func testBytes() {
    let otherBytes = Array((str+"abc").utf8)

    withUTF8Span { utf8Span in
      utf8Span.withUnsafeBufferPointer {
        expectEqualSequence(str.utf8, $0, stackTrace: loc)
      }
    }

    // NOTE: There's a slight jarring due to not having the same 
    // iterators for code units
  }

  func testScalars() {
    withUTF8Span { utf8Span in 
      // Test forwards
      var utf8SpanIter = utf8Span._makeScalarIterator()
      var stringIter = str.unicodeScalars.makeIterator()
      while let scalar = utf8SpanIter.next() {
        expectEqual(scalar, stringIter.next(), stackTrace: loc)
      }
      expectNil(stringIter.next(), stackTrace: loc)

      // Test backwards
      var stringRevIter = str.unicodeScalars.reversed().makeIterator()
      while let scalar = utf8SpanIter.previous() {
        expectEqual(scalar, stringRevIter.next(), stackTrace: loc)
      }
      expectNil(stringRevIter.next(), stackTrace: loc)

    }
  }

  func testCharacters() {
    withUTF8Span { utf8Span in 
      // Test forwards
      var utf8SpanIter = utf8Span._makeCharacterIterator()
      var stringIter = str.makeIterator()
      while let char = utf8SpanIter.next() {
        expectEqual(char, stringIter.next(), stackTrace: loc)
      }
      expectNil(stringIter.next(), stackTrace: loc)

      // Test backwards
      var stringRevIter = str.reversed().makeIterator()
      while let char = utf8SpanIter.previous() {
        expectEqual(char, stringRevIter.next(), stackTrace: loc)
      }
      expectNil(stringRevIter.next(), stackTrace: loc)
    }
  }

  func run() {
    testBytes()
    testScalars()
    testCharacters()
  }

}

if #available(SwiftStdlib 6.1, *) {
  suite.test("UTF8Span/iterators") {
    func test(
      _ s: String,
      file: String = #file,
      line: UInt = #line
    ) {
      print("testing: \(s)")
      let t = ContentEquivalenceTestCase(
        str: s, loc: .init(SourceLoc(file, line)))
      t.run()
    }

    test("abc")
    test("abcde\u{301}")
    test("abcde\u{301}")
    test("abéÏ𓀀")
    test("012345678901234567890")
    test("abéÏ012345678901234567890𓀀")
    test("😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦")
    test("")
  }
}

