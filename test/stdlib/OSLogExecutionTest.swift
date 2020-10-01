// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 5 -DPTR_SIZE_%target-ptrsize -o %t/OSLogExecutionTest
// RUN: %target-codesign %t/OSLogExecutionTest
// RUN: %target-run %t/OSLogExecutionTest
//
// RUN: %target-build-swift %s -O -swift-version 5 -DPTR_SIZE_%target-ptrsize -o %t/OSLogExecutionTest
// RUN: %target-codesign %t/OSLogExecutionTest
// RUN: %target-run %t/OSLogExecutionTest
// REQUIRES: executable_test
//
// REQUIRES: VENDOR=apple

// Run-time tests for testing the correctness of the optimizations that optimize the
// construction of the format string and the byte buffer from a string interpolation.
// The tests here are run with -Onone (which includes only mandatory optimizations)
// and also with full optimizations -O.

import OSLogTestHelper
import StdlibUnittest
import Foundation

defer { runAllTests() }

internal var InterpolationTestSuite = TestSuite("OSLogInterpolationTest")
internal let bitsPerByte = 8

/// A struct that provides methods for checking whether a given byte buffer
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
    case autoFlag = 0
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

  /// Check the encoding of an argument headers in the byte buffer starting from
  /// the `startIndex` and return argument bytes.
  private func checkArgumentHeadersAndGetBytes(
    startIndex: Int,
    size: UInt8,
    flag: ArgumentFlag,
    type: ArgumentType
  ) -> [UInt8] {
    let argumentHeader = buffer[startIndex]
    expectEqual((type.rawValue << 4) | flag.rawValue, argumentHeader)
    expectEqual(size, buffer[startIndex + 1])
    // Argument data starts after the two header bytes.
    let argumentBytes: [UInt8] =
      (0..<Int(size)).reduce(into: []) { (acc, index) in
        acc.append(buffer[startIndex + 2 + index])
      }
    return argumentBytes
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding for a
  /// numerical value.
  internal func checkNumeric<T>(
    startIndex: Int,
    flag: ArgumentFlag,
    type: ArgumentType,
    expectedValue: T
  ) where T : Numeric {
    let byteSize = UInt8(MemoryLayout<T>.size)
    let argumentBytes =
      checkArgumentHeadersAndGetBytes(
        startIndex: startIndex,
        size: byteSize,
        flag: flag,
        type: type)
    withUnsafeBytes(of: expectedValue) { expectedBytes in
      for i in 0..<Int(byteSize) {
        expectEqual(
          expectedBytes[i],
          argumentBytes[i],
          "mismatch at byte number \(i) "
            + "of the expected value \(expectedValue)")
      }
    }
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding for an Int.
  internal func checkInt<T>(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedInt: T
  ) where T : FixedWidthInteger {
    checkNumeric(
      startIndex: startIndex,
      flag: flag,
      type: .scalar,
      expectedValue: expectedInt)
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding for a count.
  internal func checkCount(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedCount: Int
  ) {
    checkNumeric(
      startIndex: startIndex,
      flag: flag,
      type: .count,
      expectedValue: CInt(expectedCount))
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding
  /// for a string.
  internal func checkString(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedString: String
  ) {
    let pointerSize = UInt8(MemoryLayout<UnsafePointer<Int8>>.size)
    let argumentBytes =
      checkArgumentHeadersAndGetBytes(
        startIndex: startIndex,
        size: pointerSize,
        flag: flag,
        type: .string)
    // Read the pointer to a string stored in the buffer and compare it with
    // the expected string using `strcmp`. Note that it is important we use a
    // C function here to compare the string as it more closely represents
    // the C os_log functions.
    var stringAddress: Int = 0
    // Copy the bytes of the address byte by byte. Note that
    // RawPointer.load(fromByteOffset:,_) function cannot be used here as the
    // address: `buffer + offset` is not aligned for reading an Int.
    for i in 0..<Int(pointerSize) {
      stringAddress |= Int(argumentBytes[i]) << (8 * i)
    }
    let bufferDataPointer = UnsafePointer<Int8>(bitPattern: stringAddress)
    expectedString.withCString {
      let compareResult = strcmp($0, bufferDataPointer)
      expectEqual(0, compareResult, "strcmp returned \(compareResult)")
    }
  }

  /// Check whether the bytes starting from `startIndex` contain the encoding
  /// for an NSObject.
  internal func checkNSObject(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedObject: NSObject
  ) {
    let pointerSize = UInt8(MemoryLayout<UnsafePointer<Int8>>.size)
    let argumentBytes =
      checkArgumentHeadersAndGetBytes(
        startIndex: startIndex,
        size: pointerSize,
        flag: flag,
        type: .object)
    // Convert data to a pointer and check if the addresses stored in the
    // pointer and the one in the buffer match.
    let objectAddress =
      Unmanaged
        .passUnretained(expectedObject)
        .toOpaque()
    withUnsafeBytes(of: objectAddress) { expectedBytes in
      for i in 0..<Int(pointerSize) {
        // Argument data starts after the two header bytes.
        expectEqual(
          expectedBytes[i],
          argumentBytes[i],
          "mismatch at byte number \(i) "
            + "of the expected object address \(objectAddress)")
      }
    }
  }

  internal func checkDouble(
    startIndex: Int,
    flag: ArgumentFlag,
    expectedValue: Double
  ) {
    let byteSize: UInt8 = 8
    let argumentBytes =
      checkArgumentHeadersAndGetBytes(
        startIndex: startIndex,
        size: byteSize,
        flag: flag,
        type: .scalar)
    withUnsafeBytes(of: expectedValue) { expectedBytes in
      for i in 0..<Int(byteSize) {
        expectEqual(
          expectedBytes[i],
          argumentBytes[i],
          "mismatch at byte number \(i) "
            + "of the expected value \(expectedValue)")
      }
    }
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
  _osLogTestHelper(
    "An integer literal \(10)",
    assertion: { (formatString, buffer) in
    expectEqual(
      "An integer literal %ld",
      formatString)

    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 1,
      hasPrivate: false,
      hasNonScalar: false)

    bufferChecker.checkArguments({
      bufferChecker.checkInt(startIndex: $0, flag: .autoFlag, expectedInt: 10)
    })
  })
}

InterpolationTestSuite.test("integer with formatting") {
  _osLogTestHelper(
    "Minimum integer value: \(UInt.max, format: .hex)",
    assertion: { (formatString, buffer) in
      expectEqual(
        "Minimum integer value: %lx",
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 1,
        hasPrivate: false,
        hasNonScalar: false)

      bufferChecker.checkArguments({
        bufferChecker.checkInt(
          startIndex: $0,
          flag: .autoFlag,
          expectedInt: UInt.max)
      })
  })
}

InterpolationTestSuite.test("integer with privacy and formatting") {
  let addr: UInt = 0x7afebabe
  _osLogTestHelper(
    "Access to invalid address: \(addr, format: .hex, privacy: .private)",
    assertion: { (formatString, buffer) in
      expectEqual(
        "Access to invalid address: %{private}lx",
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
  let filePerms: UInt = 0o777
  let pid = 122225
  let privateID = 0x79abcdef

  _osLogTestHelper(
    """
    Access prevented: process \(pid, privacy: .public) initiated by \
    user: \(privateID, privacy: .private) attempted resetting \
    permissions to \(filePerms, format: .octal)
    """,
    assertion: { (formatString, buffer) in
      expectEqual(
        """
        Access prevented: process %{public}ld initiated by \
        user: %{private}ld attempted resetting permissions \
        to %lo
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
          flag: .autoFlag,
          expectedInt: filePerms)
        })
  })
}

InterpolationTestSuite.test("interpolation of too many arguments") {
  // The following string interpolation has 49 interpolated values. Only 48
  // of these must be present in the generated format string and byte buffer.
  _osLogTestHelper(
    """
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
    \(1) \(1) \(1) \(1) \(1) \(1) \(1)
    """,
    assertion: { (formatString, buffer) in
      expectEqual(
        String(
          repeating: "%ld ",
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
              flag: .autoFlag,
              expectedInt: 1) },
          count: Int(maxOSLogArgumentCount))
      )
  })
}

InterpolationTestSuite.test("string interpolations with percents") {
  _osLogTestHelper(
    "a = (c % d)%%",
    assertion: { (formatString, buffer) in

      expectEqual("a = (c %% d)%%%%", formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 0,
        hasPrivate: false,
        hasNonScalar: false)
  })
}

InterpolationTestSuite.test("integer types") {
  _osLogTestHelper("Int32 max: \(Int32.max)") {
    (formatString, buffer) in
    expectEqual("Int32 max: %d", formatString)

    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 1,
      hasPrivate: false,
      hasNonScalar: false)

    bufferChecker.checkArguments({
      bufferChecker.checkInt(
        startIndex: $0,
        flag: .autoFlag,
        expectedInt: Int32.max)
    })
  }
}

InterpolationTestSuite.test("string arguments") {
  let small = "a"
  let large = "this is a large string"
  _osLogTestHelper(
    "small: \(small, privacy: .public) large: \(large)") {
    (formatString, buffer) in
      expectEqual("small: %{public}s large: %s", formatString)

    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 2,
      hasPrivate: false,
      hasNonScalar: true
    )

    bufferChecker.checkArguments({
      bufferChecker.checkString(
        startIndex: $0,
        flag: .publicFlag,
        expectedString: small)
    },
    { bufferChecker.checkString(
      startIndex: $0,
      flag: .autoFlag,
      expectedString: large)
    })
  }
}

InterpolationTestSuite.test("dynamic strings") {
  let concatString = "hello" + " - " + "world"
  let interpolatedString = "\(31) trillion digits of pi are known so far"

  _osLogTestHelper(
    """
    concat: \(concatString, privacy: .public) \
    interpolated: \(interpolatedString, privacy: .private)
    """) { (formatString, buffer) in
      expectEqual("concat: %{public}s interpolated: %{private}s", formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 2,
        hasPrivate: true,
        hasNonScalar: true
      )

      bufferChecker.checkArguments({
        bufferChecker.checkString(
          startIndex: $0,
          flag: .publicFlag,
          expectedString: concatString)
      },
      { bufferChecker.checkString(
          startIndex: $0,
          flag: .privateFlag,
          expectedString: interpolatedString)
      })
  }
}

InterpolationTestSuite.test("NSObject") {
  let nsArray: NSArray = [0, 1, 2]
  let nsDictionary: NSDictionary = [1 : ""]

  _osLogTestHelper(
    """
    NSArray: \(nsArray, privacy: .public) \
    NSDictionary: \(nsDictionary)
    """) { (formatString, buffer) in
      expectEqual("NSArray: %{public}@ NSDictionary: %@", formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 2,
        hasPrivate: false,
        hasNonScalar: true
      )

      bufferChecker.checkArguments({
        bufferChecker.checkNSObject(
          startIndex: $0,
          flag: .publicFlag,
          expectedObject: nsArray)
      },
      { bufferChecker.checkNSObject(
          startIndex: $0,
          flag: .autoFlag,
          expectedObject: nsDictionary)
      })
  }
}

// A generic function.
func toString<T>(_ subject: T?) -> String {
  return ""
}

protocol TestProto {
}

InterpolationTestSuite.test("Interpolation of complex expressions") {
  class TestClass<T: TestProto>: NSObject {
    func testFunction() {
      // The following call should not crash.
      _osLogTestHelper("A complex expression \(toString(self))") {
        (formatString, _) in
        expectEqual("A complex expression %s", formatString)
      }
    }
  }
  class B : TestProto { }
  TestClass<B>().testFunction()
}

InterpolationTestSuite.test("Include prefix formatting option") {
  let unsignedValue: UInt = 0o171
  _osLogTestHelper(
  "Octal with prefix: \(unsignedValue, format: .octal(includePrefix: true))") {
    (formatString, buffer) in
    expectEqual("Octal with prefix: 0o%lo", formatString)
  }
}

InterpolationTestSuite.test("Hex with uppercase formatting option") {
  let unsignedValue: UInt = 0xcafebabe
  _osLogTestHelper(
  "Hex with uppercase: \(unsignedValue, format: .hex(uppercase: true))") {
    (formatString, buffer) in
    expectEqual("Hex with uppercase: %lX", formatString)
  }
}

InterpolationTestSuite.test("Integer with explicit positive sign") {
  let posValue = Int.max
  _osLogTestHelper(
  "\(posValue, format: .decimal(explicitPositiveSign: true))") {
    (formatString, buffer) in
    expectEqual("%+ld", formatString)
  }
}

InterpolationTestSuite.test("Unsigned integer with explicit positive sign") {
  let posValue = UInt.max
  _osLogTestHelper(
  "\(posValue, format: .decimal(explicitPositiveSign: true))") {
    (formatString, buffer) in
    expectEqual("+%lu", formatString)
  }
}

InterpolationTestSuite.test("Integer formatting with precision") {
  let intValue = 1200
  _osLogTestHelper(
  "\(intValue, format: .decimal(minDigits: 10))") {
    (formatString, buffer) in
    expectEqual("%.*ld", formatString)

    // The buffer should contain two arguments: precision and the actual argument.
    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 2,
      hasPrivate: false,
      hasNonScalar: false
    )
    bufferChecker.checkArguments(
     { bufferChecker.checkCount(
         startIndex: $0,
         flag: .autoFlag,
         expectedCount: 10)
     },
     { bufferChecker.checkInt(
         startIndex: $0,
         flag: .autoFlag,
         expectedInt: intValue)
     })
  }
}

InterpolationTestSuite.test("Integer formatting with alignment") {
  let intValue = 10
  _osLogTestHelper(
    """
    \(intValue, align: .right(columns: 10)) \
    \(intValue, align: .left(columns: 5), privacy: .private)
    """) {
    (formatString, buffer) in
      expectEqual("%*ld %{private}-*ld", formatString)
  }
}

InterpolationTestSuite.test("Integer formatting with precision and alignment") {
  let intValue = 1200
  _osLogTestHelper(
  "\(intValue, format: .decimal(minDigits: 10), align: .left(columns: 7))") {
    (formatString, buffer) in
    expectEqual("%-*.*ld", formatString)

    // The buffer must contain three arguments: width, precision, and the argument.
    let bufferChecker = OSLogBufferChecker(buffer)
    bufferChecker.checkSummaryBytes(
      argumentCount: 3,
      hasPrivate: false,
      hasNonScalar: false
    )
    bufferChecker.checkArguments(
     { bufferChecker.checkCount(
         startIndex: $0,
         flag: .autoFlag,
         expectedCount: 7)
     },
     { bufferChecker.checkCount(
         startIndex: $0,
         flag: .autoFlag,
         expectedCount: 10)
     },
     { bufferChecker.checkInt(
         startIndex: $0,
         flag: .autoFlag,
         expectedInt: intValue)
     })
  }
}

InterpolationTestSuite.test("String with alignment") {
  let smallString = "a" + "b"
  let concatString = "hello" + " - " + "world"
  _osLogTestHelper(
    """
    \(smallString, align: .right(columns: 10)) \
    \(concatString, align: .left(columns: 7), privacy: .public)
    """) {
    (formatString, buffer) in
      expectEqual("%*s %{public}-*s", formatString)
  }
}

InterpolationTestSuite.test("Floats and Doubles") {
  let x = 1.2 + 0.5
  let pi: Double = 3.141593
  let floatPi: Float = 3.141593
  _osLogTestHelper(
    """
    A double value: \(x, privacy: .private) \
    pi as double: \(pi), pi as float: \(floatPi)
    """) {
    (formatString, buffer) in
      expectEqual(
        """
        A double value: %{private}f pi as double: %f, \
        pi as float: %f
        """,
        formatString)

      let bufferChecker = OSLogBufferChecker(buffer)
      bufferChecker.checkSummaryBytes(
        argumentCount: 3,
        hasPrivate: true,
        hasNonScalar: false)

      bufferChecker.checkArguments(
        { bufferChecker.checkDouble(
          startIndex: $0,
          flag: .privateFlag,
          expectedValue: x)
        },
        { bufferChecker.checkDouble(
          startIndex: $0,
          flag: .autoFlag,
          expectedValue: pi)
        },
        { bufferChecker.checkDouble(
          startIndex: $0,
          flag: .autoFlag,
          expectedValue: Double(floatPi))
        })
  }
}
