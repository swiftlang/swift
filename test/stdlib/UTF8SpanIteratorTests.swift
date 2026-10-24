// RUN: %target-run-stdlib-swift(-enable-experimental-feature LifetimeDependence) %S/Inputs/
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var suite = TestSuite("UTF8SpanIterator")
defer { runAllTests() }

@available(SwiftStdlib 6.2, *)
extension Array {
  func withSpan<R>(_ f: (Span<Element>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  func withSpan<R>(_ f: (Span<UInt8>) throws -> R) rethrows -> R {
    try self._withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
}


//
@available(SwiftStdlib 6.2, *)
struct ContentEquivalenceTestCase {
  var str: String
  var loc: SourceLocStack
}

@available(SwiftStdlib 6.2, *)
extension ContentEquivalenceTestCase {
  func expectStart(
    _ scalars: inout UTF8Span.UnicodeScalarIterator
  ) {
    let firstScalar = str.unicodeScalars.first
    expectEqual(0, scalars.currentCodeUnitOffset, stackTrace: loc)
    expectNil(scalars.previous(), stackTrace: loc)
    expectEqual(firstScalar, scalars.next(), stackTrace: loc)
    expectEqual(firstScalar, scalars.previous(), stackTrace: loc)
    expectNil(scalars.previous(), stackTrace: loc)
  }

  func expectEnd(
    _ scalars: inout UTF8Span.UnicodeScalarIterator
  ) {
    let lastScalar = str.unicodeScalars.last
    expectEqual(scalars.currentCodeUnitOffset, scalars.codeUnits.count, stackTrace: loc)
    expectNil(scalars.next(), stackTrace: loc)
    expectEqual(lastScalar, scalars.previous(), stackTrace: loc)
    expectEqual(lastScalar, scalars.next(), stackTrace: loc)
    expectNil(scalars.next(), stackTrace: loc)
  }

  func expectStart(
    _ chars: inout UTF8Span.CharacterIterator
  ) {
    let firstChar = str.first
    expectEqual(0, chars.currentCodeUnitOffset, stackTrace: loc)
    expectNil(chars.previous(), stackTrace: loc)
    expectEqual(firstChar, chars.next(), stackTrace: loc)
    expectEqual(firstChar, chars.previous(), stackTrace: loc)
    expectNil(chars.previous(), stackTrace: loc)
  }

  func expectEnd(
    _ chars: inout UTF8Span.CharacterIterator
  ) {
    let lastChar = str.last
    expectEqual(chars.currentCodeUnitOffset, chars.codeUnits.count, stackTrace: loc)
    expectNil(chars.next(), stackTrace: loc)
    expectEqual(lastChar, chars.previous(), stackTrace: loc)
    expectEqual(lastChar, chars.next(), stackTrace: loc)
    expectNil(chars.next(), stackTrace: loc)
  }


  func withUTF8Span<R>(_ f: (UTF8Span) throws -> R) rethrows -> R {
    try Array(str.utf8).withSpan { span in
      try f(try! UTF8Span(validating: span))
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension ContentEquivalenceTestCase {
  func testBytes() {
    let otherBytes = Array((str+"abc").utf8)

    withUTF8Span { utf8Span in
      utf8Span._withUnsafeBufferPointer {
        expectEqualSequence(str.utf8, $0, stackTrace: loc)
      }
    }

    // NOTE: There's a slight jarring due to not having the same
    // iterators for code units
  }

  func testScalars() {
    withUTF8Span { utf8Span in
      // Test forwards
      var utf8SpanIter = utf8Span.makeUnicodeScalarIterator()
      var stringIter = str.unicodeScalars.makeIterator()
      while let scalar = utf8SpanIter.next() {
        expectEqual(scalar, stringIter.next(), stackTrace: loc)
      }
      expectNil(stringIter.next(), stackTrace: loc)
      expectEnd(&utf8SpanIter)

      // Test backwards
      var stringRevIter = str.unicodeScalars.reversed().makeIterator()
      while let scalar = utf8SpanIter.previous() {
        expectEqual(scalar, stringRevIter.next(), stackTrace: loc)
      }
      expectNil(stringRevIter.next(), stackTrace: loc)
      expectStart(&utf8SpanIter)

      let numElements = str.unicodeScalars.count
      let lastElement = str.unicodeScalars.last
      let firstElement = str.unicodeScalars.first

      expectEqual(numElements, utf8SpanIter.skipForward(by: Int.max))
      expectEnd(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipBack(by: Int.max))
      expectStart(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipForward(by: numElements))
      expectEnd(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipBack(by: numElements))
      expectStart(&utf8SpanIter)

      if numElements > 0 {
        expectStart(&utf8SpanIter)
        expectEqual(numElements-1, utf8SpanIter.skipForward(by: numElements-1))
        expectEqual(lastElement, utf8SpanIter.next())
        expectEnd(&utf8SpanIter)

        expectEqual(numElements-1, utf8SpanIter.skipBack(by: numElements-1))
        expectEqual(firstElement, utf8SpanIter.previous())
        expectStart(&utf8SpanIter)
      }

      // TODO: test reset variants
      // TODO: test prefix/suffix

    }
  }

  func testCharacters() {
    withUTF8Span { utf8Span in
      // Test forwards
      var utf8SpanIter = utf8Span.makeCharacterIterator()
      var stringIter = str.makeIterator()
      while let char = utf8SpanIter.next() {
        expectEqual(char, stringIter.next(), stackTrace: loc)
      }
      expectNil(stringIter.next(), stackTrace: loc)
      expectEnd(&utf8SpanIter)

      // Test backwards
      var stringRevIter = str.reversed().makeIterator()
      while let char = utf8SpanIter.previous() {
        expectEqual(char, stringRevIter.next(), stackTrace: loc)
      }
      expectNil(stringRevIter.next(), stackTrace: loc)
      expectStart(&utf8SpanIter)

      let numElements = str.count
      let lastElement = str.last
      let firstElement = str.first

      expectEqual(numElements, utf8SpanIter.skipForward(by: Int.max))
      expectEnd(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipBack(by: Int.max))
      expectStart(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipForward(by: numElements))
      expectEnd(&utf8SpanIter)
      expectEqual(numElements, utf8SpanIter.skipBack(by: numElements))
      expectStart(&utf8SpanIter)

      if numElements > 0 {
        expectStart(&utf8SpanIter)
        expectEqual(numElements-1, utf8SpanIter.skipForward(by: numElements-1))
        expectEqual(lastElement, utf8SpanIter.next())
        expectEnd(&utf8SpanIter)

        expectEqual(numElements-1, utf8SpanIter.skipBack(by: numElements-1))
        expectEqual(firstElement, utf8SpanIter.previous())
        expectStart(&utf8SpanIter)
      }

      // TODO: test reset variants
      // TODO: test prefix/suffix
    }
  }

  func testBoundsChecks() {
    var count = str.utf8.count
    let span = str.utf8Span
    expectEqual(count, span.count)

    var scalarIter = span.makeUnicodeScalarIterator()
    var charIter = span.makeCharacterIterator()

    // Rounding: count is invariant
    scalarIter.reset(roundingForwardsFrom: count)
    expectEqual(count, scalarIter.currentCodeUnitOffset)
    scalarIter.reset(roundingBackwardsFrom: count)
    expectEqual(count, scalarIter.currentCodeUnitOffset)

    charIter.reset(roundingForwardsFrom: count)
    expectEqual(count, charIter.currentCodeUnitOffset)
    charIter.reset(roundingBackwardsFrom: count)
    expectEqual(count, charIter.currentCodeUnitOffset)

    // Rounding: 0 is invariant
    scalarIter.reset(roundingForwardsFrom: 0)
    expectEqual(0, scalarIter.currentCodeUnitOffset)
    scalarIter.reset(roundingBackwardsFrom: 0)
    expectEqual(0, scalarIter.currentCodeUnitOffset)

    charIter.reset(roundingForwardsFrom: 0)
    expectEqual(0, charIter.currentCodeUnitOffset)
    charIter.reset(roundingBackwardsFrom: 0)
    expectEqual(0, charIter.currentCodeUnitOffset)

    // Trap for positions outside of 0..<count
    expectCrash {
      scalarIter.reset(roundingForwardsFrom: count+1)
    }
    expectCrash {
      scalarIter.reset(roundingForwardsFrom: count+1_000_000)
    }
    expectCrash {
      scalarIter.reset(roundingForwardsFrom: -1)
    }
    expectCrash {
      scalarIter.reset(roundingForwardsFrom: -count)
    }

    expectCrash {
      scalarIter.reset(roundingBackwardsFrom: count+1)
    }
    expectCrash {
      scalarIter.reset(roundingBackwardsFrom: count+1_000_000)
    }
    expectCrash {
      scalarIter.reset(roundingBackwardsFrom: -1)
    }
    expectCrash {
      scalarIter.reset(roundingBackwardsFrom: -count)
    }

    expectCrash {
      charIter.reset(roundingForwardsFrom: count+1)
    }
    expectCrash {
      charIter.reset(roundingForwardsFrom: count+1_000_000)
    }
    expectCrash {
      charIter.reset(roundingForwardsFrom: -1)
    }
    expectCrash {
      charIter.reset(roundingForwardsFrom: -count)
    }

    expectCrash {
      charIter.reset(roundingBackwardsFrom: count+1)
    }
    expectCrash {
      charIter.reset(roundingBackwardsFrom: count+1_000_000)
    }
    expectCrash {
      charIter.reset(roundingBackwardsFrom: -1)
    }
    expectCrash {
      charIter.reset(roundingBackwardsFrom: -count)
    }
  }

  func run() {
    testBytes()
    testScalars()
    testCharacters()

    // Tests that have expected crashes
    testBoundsChecks()
  }

}

if #available(SwiftStdlib 6.2, *) {
  suite.test("UTF8Span/iterators") {
    func test(
      _ s: String,
      file: String = #file,
      line: UInt = #line
    ) {
      // print("testing: \(s)")
      let t = ContentEquivalenceTestCase(
        str: s, loc: .init(SourceLoc(file, line)))
      t.run()
    }

    test("")
    test("a")
    test("á")
    test("a\u{301}")
    test("🧟‍♀️")
    test("abc")
    test("abcde\u{301}")
    test("abéÏ𓀀")
    test("012345678901234567890")
    test("abéÏ012345678901234567890𓀀")
    test("😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦")
    test("defghijklmnopqrstuvwxyz")
    test("ab🧟‍♀️de\u{301}bytés")
    test("ab🧟‍♀️de\u{301}🧟‍♀️")
    test("ab🧟‍♀️de🧟‍♀️\u{301}")
  }
}

// @available(SwiftStdlib 6.2, *)
// extension UTF8Span {
//   func splitOffASCIIPrefix() -> (UTF8Span, UTF8Span) {
//     if isKnownASCII {
//       return (self, .init())
//     }
//     var splitPoint = 0
//     while splitPoint < codeUnits.count && codeUnits[unchecked: split] < 0x80 {
//       splitPoint += 1
//     }

//   }
// }

if #available(SwiftStdlib 6.2, *) {
  suite.test("UTF8Span/whatever") {
    // var badURLBytes: [UInt8] = []
    // badURLBytes.append(contentsOf: "http://servername/scripts/..".utf8)

    // // Invalid overlong encoding of "/"
    // badURLBytes.append(contentsOf: [0xC0, 0xAF])

    // badURLBytes.append(contentsOf: "../winnt/system32/cmd.exe".utf8)

    // // try! UTF8Span(validating: badURLBytes.span)

    // badURLBytes.withSpan {
    //   try! UTF8Span(validating: $0)
    // }



  }
}


