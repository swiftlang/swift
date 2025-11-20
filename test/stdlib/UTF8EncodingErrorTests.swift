// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test

// FIXME: this test is currently broken

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
    _ range: Range<Int>, errorStart: Bool = true
  ) -> Self {
    Self(UTF8.ValidationError(.truncatedScalar, range), errorStart: errorStart)
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
          // print("extracting \(currentPos)")
          _ = try UTF8Span(validating: span._extracting(currentPos...))

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
          // print(currentPos)
          // print(error)

          // print(error.byteOffsets._offset(by: currentPos))


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
          _ = try UTF8Span(validating: span._extracting(start...))
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

if #available(SwiftStdlib 6.2, *) {
  suite.test("UTF8Span/encoding errors") {
    func test(
      _ bytes: Array<UInt8>,
      _ file: String = #file, line: UInt = #line,
      _ errors: ValidationError...
    ) {
      ValidationTestCase(
        bytes, file: file, line: line, errors
      ).run()
    }

    // Valid string
    // test(Array("abcde\u{301}f😀🇺🇸🧟‍♀️🧟‍♀️".utf8), [])

    // Bad URL
    // test(
    //   Array("http://servername/scripts/..".utf8)
    //   + [0xC0, 0xAF]
    //   + Array("../winnt/system32/cmd.exe".utf8),
    //   [.overlongEncodingByte(at: 28),                    // C0
    //    .overlongEncodingByte(at: 29, errorStart: false), // AF
    //   ])

    // test(
    //   [0xC0, 0xAF, 0xE0, 0x80, 0xBF, 0xF0, 0x81, 0x82, 0x41],
    //   [.overlongEncodingByte(at: 0),                    // C0
    //    .overlongEncodingByte(at: 1, errorStart: false), // AF
    //    .overlongEncodingByte(at: 2),                    // E0
    //    .overlongEncodingByte(at: 3, errorStart: false), // 80
    //    .overlongEncodingByte(at: 4, errorStart: false), // BF
    //    .overlongEncodingByte(at: 5),                    // F0
    //    .overlongEncodingByte(at: 6, errorStart: false), // 81
    //    .overlongEncodingByte(at: 7, errorStart: false), // 82
    //   ])
    // test(
    //   [0x41, 0xC0, 0xAF, 0x41, 0xF4, 0x80, 0x80, 0x41],
    //   [.overlongEncodingByte(at: 1),                    // C0
    //    .overlongEncodingByte(at: 2, errorStart: false), // AF
    //    .truncatedScalar(4...6),                         // F4 80 80
    //   ])
    // test(
    //   [0xED, 0xAF, 0x41],
    //   [.surrogateCodePointByte(at: 0),                    // ED
    //    .surrogateCodePointByte(at: 1, errorStart: false), // AF
    //   ])
    // test(
    //   [0xED, 0xA0, 0x80, 0xED, 0xBF, 0xBF, 0xED, 0xAF, 0x41],
    //   [.surrogateCodePointByte(at: 0),                    // ED
    //    .surrogateCodePointByte(at: 1, errorStart: false), // A0
    //    .surrogateCodePointByte(at: 2, errorStart: false), // 80
    //    .surrogateCodePointByte(at: 3),                    // ED
    //    .surrogateCodePointByte(at: 4, errorStart: false), // BF
    //    .surrogateCodePointByte(at: 5, errorStart: false), // BF
    //    .surrogateCodePointByte(at: 6),                    // ED
    //    .surrogateCodePointByte(at: 7, errorStart: false), // AF
    //   ])
    // test(
    //   [0xF4, 0x91, 0x92, 0x93, 0xFF, 0x41, 0x80, 0xBF, 0x42],
    //   [.invalidNonSurrogateCodePointByte(at: 0),                    // F4
    //    .invalidNonSurrogateCodePointByte(at: 1, errorStart: false), // 91
    //    .invalidNonSurrogateCodePointByte(at: 2, errorStart: false), // 92
    //    .invalidNonSurrogateCodePointByte(at: 3, errorStart: false), // 93
    //    .invalidNonSurrogateCodePointByte(at: 4),                    // FF
    //    .unexpectedContinuationByte(at: 6),                          // 80
    //    .unexpectedContinuationByte(at: 7),                          // BF
    //   ])
    // test(
    //   [0xE1, 0x80, 0xE2, 0xF0, 0x91, 0x92, 0xF1, 0xBF, 0x41],
    //   [.truncatedScalar(0...1), // E1 80
    //    .truncatedScalar(2...2), // E2
    //    .truncatedScalar(3...5), // F0 91 92
    //    .truncatedScalar(6...7), // F1 BF
    //   ])
    // test(
    //   [0xE0, 0x81, 0x80],
    //   [.overlongEncodingByte(at: 0), // E0
    //    .overlongEncodingByte(at: 1, errorStart: false), // 81
    //    .overlongEncodingByte(at: 2, errorStart: false), // 80
    //   ])
  }
}
