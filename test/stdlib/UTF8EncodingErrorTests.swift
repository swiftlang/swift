// RUN: %target-run-stdlib-swift

// REQUIRES: executable_test

import Swift
import StdlibUnittest

var suite = TestSuite("UTF8.ValidationError")
defer { runAllTests() }

@available(SwiftStdlib 6.2, *)
extension Array {
  func withSpan<R>(_ f: (Span<Element>) throws -> R) rethrows -> R {
    try self.withUnsafeBufferPointer {
      try f(Span(_unsafeElements: $0))
    }
  }
}


extension Range<Int> {
  func _offset(by start: Int) -> Range<Int> {
    start + lowerBound ..< start + upperBound
  }
}

@available(SwiftStdlib 6.2, *)
private struct ValidationError {
  var error: UTF8.ValidationError

  // When fetching all errors, we'll get the error kind given. When
  // slicing in order to get the next error (e.g.
  // `UTF8Span.init(validating:))`, we'll get `.unexpectedContinuation`.
  var errorStart: Bool


  init(
    _ error: UTF8.ValidationError,
    errorStart: Bool
  ) {
    self.error = error
    self.errorStart = errorStart
  }

  public static func unexpectedContinuationByte(
    at i: Int, errorStart: Bool = true
  ) -> Self {
    Self(UTF8.ValidationError(.unexpectedContinuationByte, at: i), errorStart: errorStart)
  }

  public static func surrogateCodePointByte(
    at i: Int, errorStart: Bool = true
  ) -> Self {
    Self(UTF8.ValidationError(.surrogateCodePointByte, at: i), errorStart: errorStart)
  }

  public static func invalidNonSurrogateCodePointByte(
    at i: Int, errorStart: Bool = true
  ) -> Self {
    Self(UTF8.ValidationError(.invalidNonSurrogateCodePointByte, at: i), errorStart: errorStart)
  }

  public static func overlongEncodingByte(
    at i: Int, errorStart: Bool = true
  ) -> Self {
    Self(UTF8.ValidationError(.overlongEncodingByte, at: i), errorStart: errorStart)
  }

  public static func truncatedScalar(
    _ range: ClosedRange<Int>, errorStart: Bool = true
  ) -> Self {
    Self(
      UTF8.ValidationError(
        .truncatedScalar, range.lowerBound ..< range.upperBound + 1),
      errorStart: errorStart)
  }
}

@available(SwiftStdlib 6.2, *)
private struct ValidationTestCase {
  var bytes: [UInt8]

  // When fetching all errors, we'll get the error kind given. When
  // slicing in order to get the next error (e.g.
  // `UTF8Span.init(validating:))`, we'll get `.unexpectedContinuation`.
  var errors: [ValidationError]

  var loc: SourceLocStack

  init(
    _ bytes: [UInt8],
    file: String = #file,
    line: UInt = #line,
    _ errors: [ValidationError]
  ) {
    self.bytes = bytes
    self.errors = errors
    self.loc = .init(SourceLoc(file, line))
  }

  func fetchError(
    at i: Int, wasSliced: Bool
  ) -> UTF8.ValidationError {
    let err = errors[i]
    if wasSliced && !err.errorStart {
      return .init(.unexpectedContinuationByte, err.error.byteOffsets)
    }
    return err.error
  }

  func expect<T: Equatable>(
    _ lhs: T,
    _ rhs: T,
    file: String = #file,
    line: UInt = #line
  ) {
    expectEqual(
      lhs,
      rhs,
      stackTrace: loc.withCurrentLoc(file: file, line: line))
  }
  func fail(
    _ message: String,
    file: String = #file,
    line: UInt = #line
  ) {
    expectationFailure(
      message,
      trace: "",
      stackTrace: loc.with(.init(file, line)))
  }

  /// Test UTF8._checkAllErrors(), which matches directly against
  /// the provided expected-errors.
  func testAllErrors() {
    let caughtErrors = Array(UTF8._checkAllErrors(bytes))
    for i in 0..<Swift.min(caughtErrors.count, errors.count) {
      expect(fetchError(at: i, wasSliced: false), caughtErrors[i])
    }
    expect(caughtErrors.count, errors.count)
  }

  /// Test UTF8Span validation. Surface subsequent errors by slicing the
  /// input (which will convert the error-kind to .unexpectedContinuationByte)
  func testSpanSlicedErrors() {
    bytes.withSpan { span in
      if errors.isEmpty {
        do throws(UTF8.ValidationError) {
          // No errors expected
          _ = try UTF8Span(validating: span)
        } catch {
          fail("Unexpected error: \(error)")
        }
        return
      }

      // Check every error, by slicing (which will change error classification
      // of continuation bytes in multi-byte errors to .unexpectedContinuation)
      var currentPos = 0
      var errorIdx = 0
      while true {
        do throws(UTF8.ValidationError) {
          _ = try UTF8Span(validating: span.extracting(currentPos...))

          if errorIdx != errors.endIndex {
            fail("Expected a thrown UTF-8 encoding error")
          }
          break
        } catch {
          guard errorIdx < errors.endIndex else {
            fail("Found unexpected subsequent error \(error)")
            break
          }

          let expectedError = fetchError(at: errorIdx, wasSliced: true)
          let adjustedErr = UTF8.ValidationError(
            error.kind,
            error.byteOffsets._offset(by: currentPos)
          )
          expect(expectedError, adjustedErr)

          currentPos = adjustedErr.byteOffsets.upperBound
          errorIdx += 1
        }

      }

      // Rest of input should be error-free
      if let start = errors.last?.error.byteOffsets.upperBound,
          start < bytes.count
      {
        do throws(UTF8.ValidationError) {
          _ = try UTF8Span(validating: span.extracting(start...))
        } catch {
          fail("Found subsequent error \(error)")
        }
      }
    }
  }

  func run() {
    testSpanSlicedErrors()
    testAllErrors()
  }
}

suite.test("UTF8Span/encoding errors")
.require(.minimumStdlib(.stdlib_6_2))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }

  func test(
    _ bytes: Array<UInt8>,
    _ errors: [ValidationError],
    file: String = #file, line: UInt = #line
  ) {
    ValidationTestCase(
      bytes, file: file, line: line, errors
    ).run()
  }

  // Valid strings
  test([], [])
  test(Array("abcde\u{301}f😀🇺🇸🧟‍♀️🧟‍♀️".utf8), [])

  // The edges of the valid scalar ranges
  test([0xC2, 0x80], [])             // U+0080, shortest two-byte
  test([0xDF, 0xBF], [])             // U+07FF, longest two-byte
  test([0xE0, 0xA0, 0x80], [])       // U+0800, shortest three-byte
  test([0xED, 0x9F, 0xBF], [])       // U+D7FF, last before the surrogates
  test([0xEE, 0x80, 0x80], [])       // U+E000, first after the surrogates
  test([0xEF, 0xBF, 0xBF], [])       // U+FFFF, longest three-byte
  test([0xF0, 0x90, 0x80, 0x80], []) // U+10000, shortest four-byte
  test([0xF4, 0x8F, 0xBF, 0xBF], []) // U+10FFFF, largest scalar

  // Overstep the valid range boundaries.
  test(
    [0xC1, 0xBF, 0x41],                                // U+007F overlong
    [.overlongEncodingByte(at: 0),                     // C1
     .overlongEncodingByte(at: 1, errorStart: false),  // BF
    ]
  )
  test(
    [0xF4, 0x90, 0x80, 0x80],                                     // U+110000
    [.invalidNonSurrogateCodePointByte(at: 0),                    // F4
     .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 90
     .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 3, errorStart: false), // 80
    ]
  )

  // Invalid leading bytes
  test(
    [0xF5, 0x80, 0x80, 0x80],                                     // U+140000
    [.invalidNonSurrogateCodePointByte(at: 0),                    // F5
     .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 3, errorStart: false), // 80
    ]
  )
  test(
    [0xF8, 0x88, 0x80, 0x80, 0x80],                               // U+200000
    [.invalidNonSurrogateCodePointByte(at: 0),                    // F8
     .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 88
     .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 3, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 4, errorStart: false), // 80
    ]
  )
  test(
    [0x41, 0xFE, 0x42],
    [.invalidNonSurrogateCodePointByte(at: 1), // FE
    ]
  )
  test(
    [0xFD, 0x80, 0x80],
    [.invalidNonSurrogateCodePointByte(at: 0),                    // FD
     .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 80
     .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 80
    ]
  )
  test(
    [0xFF, 0x80],
    [.invalidNonSurrogateCodePointByte(at: 0), // FF
     .unexpectedContinuationByte(at: 1),       // 80, extra
    ]
  )

  // Unexpected continuation bytes
  test(
    [0x80],
    [.unexpectedContinuationByte(at: 0), // 80
    ]
  )
  test(
    [0xBF, 0x41],
    [.unexpectedContinuationByte(at: 0), // BF
    ]
  )

  // Truncation at the end of input
  test(
    [0xC2],
    [.truncatedScalar(0...0), // C2
    ]
  )
  test(
    [0x41, 0xE1, 0x80],
    [.truncatedScalar(1...2), // E1 80
    ]
  )
  test(
    [0xF0, 0x9F],
    [.truncatedScalar(0...1), // F0 9F
    ]
  )
  test(
    [0xF1, 0x80, 0x80],
    [.truncatedScalar(0...2), // F1 80 80
    ]
  )

  // Non-continuation second byte
  test(
    [0xE0, 0x41],
    [.overlongEncodingByte(at: 0), // E0, needs A0...BF
    ]
  )
  test(
    [0xED, 0x41],
    [.surrogateCodePointByte(at: 0), // ED, needs 80...9F
    ]
  )
  test(
    [0xF0, 0x41],
    [.overlongEncodingByte(at: 0), // F0, needs 90...BF
    ]
  )
  test(
    [0xF4, 0x41],
    [.invalidNonSurrogateCodePointByte(at: 0), // F4, needs 80...8F
    ]
  )
  test(
    [0xE1, 0x41],
    [.truncatedScalar(0...0), // E1 has no such check to fail
    ]
  )

  // Bad URL
  test(
    Array("http://servername/scripts/..".utf8)
    + [0xC0, 0xAF]
    + Array("../winnt/system32/cmd.exe".utf8),
    [.overlongEncodingByte(at: 28),                    // C0
     .overlongEncodingByte(at: 29, errorStart: false), // AF
    ]
  )

  test(
    [0xC0, 0xAF, 0xE0, 0x80, 0xBF, 0xF0, 0x81, 0x82, 0x41],
    [.overlongEncodingByte(at: 0),                    // C0
     .overlongEncodingByte(at: 1, errorStart: false), // AF
     .overlongEncodingByte(at: 2),                    // E0
     .overlongEncodingByte(at: 3, errorStart: false), // 80
     .overlongEncodingByte(at: 4, errorStart: false), // BF
     .overlongEncodingByte(at: 5),                    // F0
     .overlongEncodingByte(at: 6, errorStart: false), // 81
     .overlongEncodingByte(at: 7, errorStart: false), // 82
    ]
  )
  test(
    [0x41, 0xC0, 0xAF, 0x41, 0xF4, 0x80, 0x80, 0x41],
    [.overlongEncodingByte(at: 1),                    // C0
     .overlongEncodingByte(at: 2, errorStart: false), // AF
     .truncatedScalar(4...6),                         // F4 80 80
    ]
  )
  test(
    [0xED, 0xAF, 0x41],
    [.surrogateCodePointByte(at: 0),                    // ED
     .surrogateCodePointByte(at: 1, errorStart: false), // AF
    ]
  )
  test(
    [0xED, 0xA0, 0x80, 0xED, 0xBF, 0xBF, 0xED, 0xAF, 0x41],
    [.surrogateCodePointByte(at: 0),                    // ED
     .surrogateCodePointByte(at: 1, errorStart: false), // A0
     .surrogateCodePointByte(at: 2, errorStart: false), // 80
     .surrogateCodePointByte(at: 3),                    // ED
     .surrogateCodePointByte(at: 4, errorStart: false), // BF
     .surrogateCodePointByte(at: 5, errorStart: false), // BF
     .surrogateCodePointByte(at: 6),                    // ED
     .surrogateCodePointByte(at: 7, errorStart: false), // AF
    ]
  )

  // Adjacent errors of different kinds.
  test(
    [0xC0, 0xED, 0xA0, 0x80],
    [.overlongEncodingByte(at: 0),                      // C0
     .surrogateCodePointByte(at: 1),                    // ED
     .surrogateCodePointByte(at: 2, errorStart: false), // A0
     .surrogateCodePointByte(at: 3, errorStart: false), // 80
    ]
  )
  test(
    [0x80, 0xED, 0xA0, 0x80],
    [.unexpectedContinuationByte(at: 0),                // 80
     .surrogateCodePointByte(at: 1),                    // ED
     .surrogateCodePointByte(at: 2, errorStart: false), // A0
     .surrogateCodePointByte(at: 3, errorStart: false), // 80
    ]
  )

  // Extra continuation bytes.
  test(
    [0xED, 0xA0, 0x80, 0x80],
    [.surrogateCodePointByte(at: 0),                      // ED
     .surrogateCodePointByte(at: 1, errorStart: false),   // A0
     .surrogateCodePointByte(at: 2, errorStart: false),   // 80
     .unexpectedContinuationByte(at: 3),                  // 80, extra
    ]
  )
  test(
    [0xC0, 0xAF, 0xAF],
    [.overlongEncodingByte(at: 0),                        // C0
     .overlongEncodingByte(at: 1, errorStart: false),     // AF
     .unexpectedContinuationByte(at: 2),                  // AF, extra
    ]
  )
  test(
    [0xED, 0xA0, 0x80, 0x41, 0x80],
    [.surrogateCodePointByte(at: 0),                      // ED
     .surrogateCodePointByte(at: 1, errorStart: false),   // A0
     .surrogateCodePointByte(at: 2, errorStart: false),   // 80
     .unexpectedContinuationByte(at: 4),                  // 80, extra
    ]
  )

  test(
    [0xF4, 0x91, 0x92, 0x93, 0xFF, 0x41, 0x80, 0xBF, 0x42],
    [.invalidNonSurrogateCodePointByte(at: 0),                    // F4
     .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 91
     .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 92
     .invalidNonSurrogateCodePointByte(at: 3, errorStart: false), // 93
     .invalidNonSurrogateCodePointByte(at: 4),                    // FF
     .unexpectedContinuationByte(at: 6),                          // 80
     .unexpectedContinuationByte(at: 7),                          // BF
    ]
  )
  test(
    [0xE1, 0x80, 0xE2, 0xF0, 0x91, 0x92, 0xF1, 0xBF, 0x41],
    [.truncatedScalar(0...1), // E1 80
     .truncatedScalar(2...2), // E2
     .truncatedScalar(3...5), // F0 91 92
     .truncatedScalar(6...7), // F1 BF
    ]
  )
  test(
    [0xE0, 0x81, 0x80],
    [.overlongEncodingByte(at: 0), // E0
     .overlongEncodingByte(at: 1, errorStart: false), // 81
     .overlongEncodingByte(at: 2, errorStart: false), // 80
    ]
  )

  // The example included in UTF8.ValidationError's doc-comment
  test(
    [0x61, 0xF1, 0x80, 0x80, 0xE1, 0x80, 0xC2, 0x62],
    [.truncatedScalar(1...3), // F1 80 80
     .truncatedScalar(4...5), // E1 80
     .truncatedScalar(6...6), // C2
    ]
  )
}

suite.test("init/negative byte offset")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  expectCrashLater()
  _ = UTF8.ValidationError(.unexpectedContinuationByte, -1 ..< 0)
}

suite.test("init/empty truncatedScalar")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  expectCrashLater()
  _ = UTF8.ValidationError(.truncatedScalar, 3..<3)
}

suite.test("init/oversized truncatedScalar")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  expectCrashLater()
  _ = UTF8.ValidationError(.truncatedScalar, 0..<4)
}

suite.test("init/multi-byte single-byte kind")
.require(.minimumStdlib(.stdlib_6_2))
.require(.crashTesting)
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  expectCrashLater()
  _ = UTF8.ValidationError(.overlongEncodingByte, 0..<2)
}
