// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 5 -DPTR_SIZE_%target-ptrsize -o %t/OSLogPrototypeExecTest
// RUN: %target-run %t/OSLogPrototypeExecTest
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Run-time tests for testing the new OS log APIs that accept string
// interpolations. The new APIs are still prototypes and must be used only in
// tests.

import OSLogPrototype
import StdlibUnittest

defer { runAllTests() }

internal var OSLogTestSuite = TestSuite("OSLogTest")

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  // Following tests check whether valid log calls execute without
  // compile-time and run-time errors.

  func logMessages(_ h: Logger) {
    // Test logging of simple messages.
    h.log("A message with no data")

    // Test logging at specific levels.
    h.log(level: .debug, "Minimum integer value: \(Int.min, format: .hex)")
    h.log(level: .info, "Maximum integer value: \(Int.max, format: .hex)")

    let privateID = 0x79abcdef
    h.log(
      level: .error,
      "Private Identifier: \(privateID, format: .hex, privacy: .private)")
    let addr = 0x7afebabe
    h.log(
      level: .fault,
      "Invalid address: 0x\(addr, format: .hex, privacy: .public)")

    // Test logging with multiple arguments.
    let filePermissions = 0o777
    let pid = 122225
    h.log(
      level: .error,
      """
      Access prevented: process \(pid) initiated by \
      user: \(privateID, privacy: .private) attempted resetting \
      permissions to \(filePermissions, format: .octal)
      """)
  }

  OSLogTestSuite.test("log with default logger") {
    let h = Logger()
    logMessages(h)
  }

  OSLogTestSuite.test("log with custom logger") {
    let h =
      Logger(subsystem: "com.swift.test", category: "OSLogAPIPrototypeTest")
    logMessages(h)
  }

  OSLogTestSuite.test("escaping of percents") {
    let h = Logger()
    h.log("a = c % d")
    h.log("Process failed after 99% completion")
    h.log("Double percents: %%")
  }

  // A stress test that checks whether the log APIs handle messages with more
  // than 48 interpolated expressions. Interpolated expressions beyond this
  // limit must be ignored.
  OSLogTestSuite.test("messages with too many arguments") {
    let h = Logger()
    h.log(
      level: .error,
      """
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(48) \(49)
      """) // The number 49 should not appear in the logged message.
  }

  OSLogTestSuite.test("escape characters") {
    let h = Logger()
    h.log("\"Imagination is more important than knowledge\" - Einstein")
    h.log("\'Imagination is more important than knowledge\' - Einstein")
    h.log("Imagination is more important than knowledge \n - Einstein")
    h.log("Imagination is more important than knowledge - \\Einstein")
    h.log("The log message will be truncated here.\0 You won't see this")
  }

  OSLogTestSuite.test("unicode characters") {
    let h = Logger()
    h.log("dollar sign: \u{24}")
    h.log("black heart: \u{2665}")
    h.log("sparkling heart: \u{1F496}")
  }

  OSLogTestSuite.test("raw strings") {
    let h = Logger()
    let x = 10

    h.log(#"There is no \(interpolated) value in this string"#)
    h.log(#"This is not escaped \n"#)
    h.log(##"'\b' is a printf escape character but not in Swift"##)
    h.log(##"The interpolated value is \##(x)"##)
    h.log(#"Sparkling heart should appear in the next line. \#n \#u{1F496}"#)
  }

  OSLogTestSuite.test("integer types") {
    let h = Logger()
    h.log("Smallest 32-bit integer value: \(Int32.min, format: .hex)")
  }
}

// The following tests check the correctness of the format string and the
// byte buffer constructed by the APIs from a string interpolation.
// These tests do not perform logging and do not require the os_log ABIs to
// be available.

internal var InterpolationTestSuite = TestSuite("OSLogInterpolationTest")
internal let intPrefix = Int.bitWidth == CLongLong.bitWidth ? "ll" : ""
internal let bitsPerByte = 8

/// A struct that exposes methods for checking whether a given byte buffer
/// conforms to the format expected by the os_log ABI. This struct acts as
/// a specification of the byte buffer format.
internal struct OSLogBufferChecker {

  internal let buffer: UnsafeBufferPointer<UInt8>

  internal init(_ byteBuffer: UnsafeBufferPointer<UInt8>) {
    buffer = byteBuffer
  }

  /// Bit mask for setting bits in the peamble. The bits denoted by the bit
  /// mask indicate whether there is an argument that is private, and whether
  /// there is an argument that is non-scalar: String, NSObject or Pointer.
  internal enum PreambleBitMask: UInt8 {
    case privateBitMask = 0x1
    case nonScalarBitMask = 0x2
  }

  /// Check the summary bytes of the byte buffer.
  /// - Parameters:
  ///  - argumentCount: number of arguments expected to be in the buffer.
  ///  - hasPrivate: true iff there exists a private argument
  ///  - hasNonScalar: true iff there exists a non-scalar argument
  internal func checkSummaryBytes(
    argumentCount: UInt8,
    hasPrivate: Bool,
    hasNonScalar: Bool
  ) {
    let preamble = buffer[0]
    let privateBit = preamble & PreambleBitMask.privateBitMask.rawValue
    expectEqual(hasPrivate, privateBit != 0)

    let nonScalarBit = preamble & PreambleBitMask.nonScalarBitMask.rawValue
    expectEqual(hasNonScalar, nonScalarBit != 0)

    expectEqual(argumentCount, buffer[1])
  }

  /// The possible values for the argument flag as defined by the os_log ABI.
  /// This occupies four least significant bits of the first byte of the
  /// argument header. Two least significant bits are used to indicate privacy
  /// and the other two bits are reserved.
  internal enum ArgumentFlag: UInt8 {
    case privateFlag = 0x1
    case publicFlag = 0x2
  }

  /// The possible values for the argument type as defined by the os_log ABI.
  /// This occupies four most significant bits of the first byte of the
  /// argument header.
  internal enum ArgumentType: UInt8 {
    case scalar = 0, count, string, pointer, object
    // TODO: include wide string and errno here if needed.
  }

  /// Check the encoding of an argument in the byte buffer starting from the
  /// `startIndex`.
  ///  - precondition: `T` must be a type that is accepted by os_log ABI.
  private func checkArgument<T>(
    startIndex: Int,
    size: UInt8,
    flag: ArgumentFlag,
    type: ArgumentType,
    expectedData: T
  ) {
    let argumentHeader = buffer[startIndex]
    expectEqual((type.rawValue << 4) | flag.rawValue, argumentHeader)

    expectEqual(size, buffer[startIndex + 1])

    // Check every byte of the payload.
    withUnsafeBytes(of: expectedData) { expectedBytes in
      for i in 0..<Int(size) {
        // Argument data starts after the two header bytes.
        expectEqual(
          expectedBytes[i],
          buffer[startIndex + 2 + i],
          "mismatch at byte number \(i) of the expected value \(expectedData)")
      }
    }
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding
  /// for an Int.
  internal func checkInt<T>(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedInt: T
  ) where T : FixedWidthInteger {
    checkArgument(
      startIndex: startIndex,
      size: UInt8(MemoryLayout<T>.size),
      flag: flag,
      type: .scalar,
      expectedData: expectedInt)
  }

  /// Check the given assertions on the arguments stored in the byte buffer.
  /// - Parameters:
  ///  - assertions: one assertion for each argument stored in the byte buffer.
  internal func checkArguments(_ assertions: [(Int) -> Void]) {
    var currentArgumentIndex = 2

    for assertion in assertions {
      expectTrue(currentArgumentIndex < buffer.count)

      assertion(currentArgumentIndex)
      // Advance the index to the next argument by adding the size of the
      // current argument and the two bytes of headers.
      currentArgumentIndex += 2 + Int(buffer[currentArgumentIndex + 1])
    }
  }

  /// Check a sequence of assertions on the arguments stored in the byte buffer.
  internal func checkArguments(_ assertions: (Int) -> Void ...) {
    checkArguments(assertions)
  }
}

InterpolationTestSuite.test("integer literal") {
  _checkFormatStringAndBuffer(
    "An integer literal \(10)",
    with: { (formatString, buffer) in
    expectEqual(
      "An integer literal %{public}\(intPrefix)d",
      formatString)

    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 1,
      hasPrivate: false,
      hasNonScalar: false)

    bufferChecker.checkArguments({
      bufferChecker.checkInt(startIndex: $0, flag: .publicFlag, expectedInt: 10)
    })
  })
}

InterpolationTestSuite.test("integer with formatting") {
  _checkFormatStringAndBuffer(
    "Minimum integer value: \(Int.min, format: .hex)",
    with: { (formatString, buffer) in
      expectEqual(
        "Minimum integer value: %{public}\(intPrefix)x",
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 1,
        hasPrivate: false,
        hasNonScalar: false)

      bufferChecker.checkArguments({
        bufferChecker.checkInt(
          startIndex: $0,
          flag: .publicFlag,
          expectedInt: Int.min)
      })
  })
}

InterpolationTestSuite.test("integer with privacy and formatting") {
  let addr = 0x7afebabe
  _checkFormatStringAndBuffer(
    "Access to invalid address: \(addr, format: .hex, privacy: .private)",
    with: { (formatString, buffer) in
      expectEqual(
        "Access to invalid address: %{private}\(intPrefix)x",
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 1,
        hasPrivate: true,
        hasNonScalar: false)

      bufferChecker.checkArguments({
        bufferChecker.checkInt(
          startIndex: $0,
          flag: .privateFlag,
          expectedInt: addr)
      })
  })
}

InterpolationTestSuite.test("integer with privacy and formatting") {
  let addr = 0x7afebabe
  _checkFormatStringAndBuffer(
    "Access to invalid address: \(addr, format: .hex, privacy: .private)",
    with: { (formatString, buffer) in
      expectEqual(
        "Access to invalid address: %{private}\(intPrefix)x",
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 1,
        hasPrivate: true,
        hasNonScalar: false)

      bufferChecker.checkArguments({
        bufferChecker.checkInt(
            startIndex: $0,
            flag: .privateFlag,
            expectedInt: addr)
      })
  })
}

InterpolationTestSuite.test("test multiple arguments") {
  let filePerms = 0o777
  let pid = 122225
  let privateID = 0x79abcdef

  _checkFormatStringAndBuffer(
    """
    Access prevented: process \(pid) initiated by \
    user: \(privateID, privacy: .private) attempted resetting \
    permissions to \(filePerms, format: .octal)
    """,
    with: { (formatString, buffer) in
      expectEqual(
        """
        Access prevented: process %{public}\(intPrefix)d initiated by \
        user: %{private}\(intPrefix)d attempted resetting permissions \
        to %{public}\(intPrefix)o
        """,
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 3,
        hasPrivate: true,
        hasNonScalar: false)

      bufferChecker.checkArguments(
        {
          bufferChecker.checkInt(
            startIndex: $0,
            flag: .publicFlag,
            expectedInt: pid)
        },
        {
          bufferChecker.checkInt(
            startIndex: $0,
            flag: .privateFlag,
            expectedInt: privateID)
        },
        {
          bufferChecker.checkInt(
          startIndex: $0,
          flag: .publicFlag,
          expectedInt: filePerms)
        })
  })
}

InterpolationTestSuite.test("interpolation of too many arguments") {
  // The following string interpolation has 49 interpolated values. Only 48
  // of these must be present in the generated format string and byte buffer.
  _checkFormatStringAndBuffer(
    """
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1)
    """,
    with: { (formatString, buffer) in
      expectEqual(
        String(
          repeating: "%{public}\(intPrefix)d ",
          count: Int(maxOSLogArgumentCount)),
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: maxOSLogArgumentCount,
        hasPrivate: false,
        hasNonScalar: false)

      bufferChecker.checkArguments(
        Array(
          repeating: {
            bufferChecker.checkInt(
              startIndex: $0,
              flag: .publicFlag,
              expectedInt: 1) },
          count: Int(maxOSLogArgumentCount))
      )
  })
}

InterpolationTestSuite.test("string interpolations with percents") {
  _checkFormatStringAndBuffer(
    "a = (c % d)%%",
    with: { (formatString, buffer) in

      expectEqual("a = (c %% d)%%%%", formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 0,
        hasPrivate: false,
        hasNonScalar: false)
  })
}

InterpolationTestSuite.test("integer types") {
  _checkFormatStringAndBuffer("Int32 max: \(Int32.max)") {
    (formatString, buffer) in
    expectEqual("Int32 max: %{public}d", formatString)

    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 1,
      hasPrivate: false,
      hasNonScalar: false)

    bufferChecker.checkArguments({
      bufferChecker.checkInt(
        startIndex: $0,
        flag: .publicFlag,
        expectedInt: Int32.max)
    })
  }
}
