// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// rdar://77087867
// UNSUPPORTED: CPU=arm64_32 && OS=watchos

// rdar://104232602
// UNSUPPORTED: CPU=x86_64 && (DARWIN_SIMULATOR=ios || DARWIN_SIMULATOR=watchos || DARWIN_SIMULATOR=tvos)

import StdlibUnittest

let PrintTests = TestSuite("FloatingPointPrinting")
//
//  // An earlier version of Swift's floating-point `.description` logic
//  // used potentially locale-sensitive C library functions, hence
//  // this logic to help verify that the output does not depend on
//  // the C locale.
//  PrintTests.setUp {
//    if let localeArgIndex = CommandLine.arguments.firstIndex(of: "--locale") {
//      let locale = CommandLine.arguments[localeArgIndex + 1]
//      expectEqual("ru_RU.UTF-8", locale)
//      setlocale(LC_ALL, locale)
//    } else {
//      setlocale(LC_ALL, "")
//    }
//  }

// Check that all floating point types
// are CustomStringConvertible
PrintTests.test("CustomStringConvertible") {
  func hasDescription(_ any: Any) {
    expectTrue(any is CustomStringConvertible)
  }
  if #available(SwiftStdlib 6.0, *) {
    hasDescription(Float16(1.0))
    hasDescription(CFloat16(1.0))
  }
  hasDescription(Float(1.0))
  hasDescription(Double(1.0))
#if !os(Windows) && (arch(i386) || arch(x86_64))
  hasDescription(Float80(1.0))
#endif
  hasDescription(CFloat(1.0))
  hasDescription(CDouble(1.0))
}

// Check that all floating point types
// are CustomDebugStringConvertible
PrintTests.test("CustomDebugStringConvertible") {
  func hasDebugDescription(_ any: Any) {
    expectTrue(any is CustomDebugStringConvertible)
  }
  if #available(SwiftStdlib 6.0, *) {
    hasDebugDescription(Float16(1.0))
    hasDebugDescription(CFloat16(1.0))
  }
  hasDebugDescription(Float(1.0))
  hasDebugDescription(Double(1.0))
#if !os(Windows) && (arch(i386) || arch(x86_64))
  hasDebugDescription(Float80(1.0))
#endif
  hasDebugDescription(CFloat(1.0))
  hasDebugDescription(CDouble(1.0))
}

#if (os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64)
@available(SwiftStdlib 6.0, *) // Float16/intel
#else
@available(SwiftStdlib 5.8, *) // Regex
#endif
func testFinite(_ bitPattern: UInt16) {
  let value = Float16(bitPattern: bitPattern)
  let string = value.description
  let debugString = value.debugDescription
  // description and debugDescription should agree for finite values
  expectEqual(string, debugString)
  // test that conversion round-trips correctly
  expectEqual(value, Float16(string))
  // parse the string so we can test for shortness and closeness
  let fmt = #/(?'sign'-?)(?'int'\d+)(?:\.(?'frac'\d+))?(?:e(?'exp'[+-]\d+))?/#
  let match = try! fmt.wholeMatch(in: string)!
  let significand: Int
  let bias: Int
  if let frac = match.frac, frac != "0" {
    significand = Int(match.int + frac)!
    bias = frac.count
  } else {
    significand = Int(match.int)!
    bias = 0
  }
  let exponent = Int(match.exp ?? "0")! - bias
  let float = Float(value)
  
  let error = (float - Float(string)!).magnitude
  // If the string representation isn't exact (up to Float accuracy), try
  // the adjacent values to see if they would have been closer.
  if error != 0 {
    let up = "\(match.sign)\(significand + 1)e\(exponent)"
    let upError = (float - Float(up)!).magnitude
    expectFalse(
      upError < error || upError == error && significand % 2 == 1,
      "Float16(\(value)).description was \(string), but \(up) would be closer."
    )
    let dn = "\(match.sign)\(significand - 1)e\(exponent)"
    let dnError = (float - Float(dn)!).magnitude
    expectFalse(
      dnError < error || dnError == error && significand % 2 == 1,
      "Float16(\(value)).description was \(string), but \(dn) would be closer."
    )
  }
  
  // If the string representation isn't an exact integer, check if we could
  // have used a shorter string.
  if error != 0 || match.exp != nil {
    let dn = "\(match.sign)\(significand/10)e\(exponent+1)"
    expectFalse(
      Float16(dn)! == value,
      "Float16(\(value)).description was \(string), but \(dn) rounds to the same value and is shorter."
    )
    let up = "\(match.sign)\((significand+9)/10)e\(exponent+1)"
    expectFalse(
      Float16(up)! == value,
      "Float16(\(value)).description was \(string), but \(up) rounds to the same value and is shorter."
    )
  }
}

PrintTests.test("Printable_Float16") {
#if (os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64)
  guard #available(SwiftStdlib 6.0, *) else { return } // Float16/intel
#else
  guard #available(SwiftStdlib 5.8, *) else { return } // Regex
#endif
  for bitPattern in UInt16.zero ..< 0x7c00 {
    testFinite(bitPattern)
    testFinite(0x8000 | bitPattern)
  }
  
  expectEqual(Float16.infinity.description, "inf")
  expectEqual((-Float16.infinity).description, "-inf")
  expectEqual(Float16.infinity.debugDescription, "inf")
  expectEqual((-Float16.infinity).debugDescription, "-inf")
  
  for bitPattern in (0x7c01 as UInt16) ... 0x7fff {
    expectEqual(Float16(bitPattern: bitPattern).description, "nan")
    expectEqual(Float16(bitPattern: 0x8000 | bitPattern).description, "nan")
    
    let payload: String = if bitPattern & 0xff == 0 {
      ""
    } else {
      "(0x\(String(bitPattern & 0xff, radix: 16)))"
    }
    let expected: String = if bitPattern & 0b10_0000_0000 == 0 {
      "snan" + payload
    } else {
      "nan" + payload
    }
    expectEqual(Float16(bitPattern: bitPattern).debugDescription, expected)
    expectEqual(Float16(bitPattern: 0x8000 | bitPattern).debugDescription, "-\(expected)")
  }
}

runAllTests()
