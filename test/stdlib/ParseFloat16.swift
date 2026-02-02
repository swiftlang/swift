// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Float16 is not supported on x86_64 macOS
// UNSUPPORTED: CPU=x86_64 && OS=macosx

// Float16 is only available in watchOS 7.0 or newer
// UNSUPPORTED: OS=watchos

// Cannot test with old OS stdlib, because that used libc strtof
// for parsing, which results in incorrect results.
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest

let tests = TestSuite("FloatingPointParsing")

fileprivate func expectRoundTrip(
  _ value: Float16,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let text = value.debugDescription
  let roundTrip = Float16(Substring(text))
  expectNotNil(roundTrip, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  if let roundTrip {
    expectEqual(roundTrip.bitPattern, value.bitPattern, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  }
}

fileprivate func expectParse(
  _ input: String,
  _ expected: Float16,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let parsed = Float16(Substring(input))
  expectNotNil(parsed, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  if let parsed {
    expectEqual(parsed.bitPattern, expected.bitPattern, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  }
}

func expectParseFails(
  _ input: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let parsed = Float16(Substring(input))
  expectNil(parsed, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
}

tests.test("Invalids") {
  expectParseFails("")
  expectParseFails("-")
  expectParseFails("+")
  expectParseFails("&")
  expectParseFails("+x")
  expectParseFails("x")
}

tests.test("Infinities") {
  expectParse("inf", Float16.infinity)
  expectParse("+inf", Float16.infinity)
  expectParse("-inf", -Float16.infinity)
  expectParse("INF", Float16.infinity)
  expectParse("InF", Float16.infinity)
  expectParse("iNf", Float16.infinity)
  expectParse("infinity", Float16.infinity)
  expectParse("INFINITY", Float16.infinity)
  expectParse("+infinity", Float16.infinity)
  expectParse("-infinity", -Float16.infinity)

  expectParseFails("i")
  expectParseFails("in")
  expectParseFails(" inf")
  expectParseFails("- inf")
  expectParseFails("--inf")
  expectParseFails("-+inf")
  expectParseFails("++inf")
  expectParseFails("inf ")
  expectParseFails("inx")
  expectParseFails("-inx")
  expectParseFails("infi")
  expectParseFails("infin")
  expectParseFails("infini")
  expectParseFails("infinit")
  expectParseFails("infinite")
  expectParseFails("infinityandbeyond")

  expectRoundTrip(Float16.infinity)
  expectRoundTrip(-Float16.infinity)
}

tests.test("NaNs") {
  // Note: Previous Swift runtime used libc strtof and then
  // truncated to Float16, which is why some of these are
  // wrong when testing previous runtimes.
  expectRoundTrip(Float16.nan)
  expectRoundTrip(-Float16.nan)
  expectRoundTrip(Float16(nan:73, signaling:false))
#if !arch(wasm32)
  expectRoundTrip(Float16(nan:73, signaling:true))
#endif
  expectParse("nan", Float16.nan)
  expectParse("NAN", Float16.nan)
  expectParse("NaN", Float16.nan)
  expectParse("-nan", -Float16.nan)
  expectParse("nan()", Float16.nan)
  expectParse("nan(0)", Float16.nan)
  expectParse("nan(000000000000000000000000000000000000000)", Float16.nan)
  expectParse("nan(0x00000000000000000000000000000000000000)", Float16.nan)
  expectParse("nan(10)", Float16(nan:10, signaling:false))
  expectParse("nan(0x10)", Float16(nan:16, signaling:false))
  expectParse("nan(010)", Float16(nan:8, signaling:false))
  expectParse("nan(9)", Float16(nan:9, signaling:false))
  expectParse("nan(99)", Float16(nan:99, signaling:false))
  expectParse("nan(255)", Float16(nan:255, signaling:false))
  expectParse("nan(256)", Float16(nan:0, signaling:false))
  expectParse("nan(511)", Float16(nan:255, signaling:false))
  expectParse("nan(999999)", Float16(nan:63, signaling:false))
  expectParse("nan(999999999999999)", Float16(nan:255, signaling:false))
  expectParseFails("n")
  expectParseFails("na")
  expectParseFails("nann")
  expectParseFails("nananananana")
}

tests.test("HexFloats") {
  expectParseFails("0x")
  expectParseFails("0x.")
  expectParseFails("0x😀")
  expectParseFails("0x1😀p2")
  expectParseFails("0x1.07😀p2")
  expectParseFails("0x1p😀")
  expectParseFails("0x1p+😀")
  expectParseFails("0x1p")
  expectParseFails("0x1p+")
  expectParseFails("0xp+7")
  expectParseFails("0x.p1")
  expectParseFails("0x..p1")
  expectParseFails("0x0p1.0")
  expectParse("0x0p0", 0.0)
  expectParse("0x0p1", 0.0)
  expectParse("-0x0p0", -0.0)
  expectParse("0x0p999999999", 0.0)
  expectParse("0x0.0p999999999", 0.0)
  expectParse("0x.0p-999999999", 0.0)
  expectParse("0x0p-999999999", 0.0)
  expectParse("0x1p-25", 0.0)
  expectParse("0x.000001", Float16.leastNonzeroMagnitude)
  expectParse("0x1p-24", Float16.leastNonzeroMagnitude)
  expectParse("0x1p-24", Float16(bitPattern:1))
  expectParse("0x1p-23", Float16(bitPattern:2))
  expectParse("0x1p-22", Float16(bitPattern:4))
  expectParse("0x1p-21", Float16(bitPattern:8))
  expectParse("0x1p-20", Float16(bitPattern:16))

  // Test the tricky rounding of values between the largest subnormal and least normal
  expectParse("0x0.ffcp-14", Float16.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x0.ffdffffp-14", Float16.leastNormalMagnitude.nextDown) // Just less than halfway
  expectParse("0x0.ffep-14", Float16.leastNormalMagnitude) // Halfway
  expectParse("0x0.ffe0000001p-14", Float16.leastNormalMagnitude) // Just above halfway
  expectParse("0x1p-14", Float16.leastNormalMagnitude) // Smallest normal

  expectParse("0x1.554p-2", 1.0/3.0)
  expectParse("0x0.ffe", 0.99951172)
  expectParse("0x0.ffeffff", 0.99951172) // Note: Old Swift implementation gets this wrong
  expectParse("0x0.fff", 1.0)
  expectParse("0x1p0", 1.0)
  expectParse("0x1.002", 1.0) // Exact halfway between 1.0 and succ(1.0), rounds even
  // Note: Old implementation gets this wrong due to double-rounding
  expectParse("0x1.0020000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
    1.00097656) // Teensy-tiny bit bigger than above, should round up
  expectParse("0x1.00200000000001", 1.00097656) // Bigger than above
  expectParse("0x1002000000000000000000001p-96", 1.00097656)
  expectParse("0x0000000000000.000000000000000000000001002000000000000000000001p96", 1.00097656)
  expectParse("0x1.004", 1.00097656)
  expectParse("0x1p+1", 2.0)
  expectParse("0x1p+0000000000000000000000000000000000001", 2.0)
  expectParse("0x12", 18.0)
  expectParse("0xab", 171.0)
  expectParse("0x1p+10", 1024.0)
  expectParse("0x1p+0000000000000000000000000010", 1024.0)
  expectParse("0x1.920p1", Float16.pi)
  expectParse("0xffe0", Float16.greatestFiniteMagnitude)
  expectParse("0xffe00000000000.0000000000000000000000000000000000000000001p-40",
    Float16.greatestFiniteMagnitude)
  expectParse("0x1.ffcp+15", Float16.greatestFiniteMagnitude)

  expectParse("0xffa0", 65440.0) // Exact (odd)
  expectParse("0xffb0", 65472.0) // Rounds up
  expectParse("0xffc0", 65472.0) // Exact (even)
  expectParse("0xffd0", 65472.0) // Rounds down
  expectParse("0xffe0", Float16.greatestFiniteMagnitude) // Exact (odd)
  // Rationale for the assertions below:
  // * Float16.greatestFiniteMagnitude has an odd significand
  // * Let epsilon = the difference between Float16.greatestFiniteMagnitude and its immediate predecessor
  // * Define a synthetic finite successor to Float16.gFM as Float16.gFM + epsilon
  // * Assertion:  the value above should round to infinity
  // * Assertion:  the value above should be treated as having an even significand
  // * Conclusion:  Exact halfway between Float16.gFM and lIM is the smallest magnitude that should round to infinity
  // Note: Old Swift implementation gets these wrong
  expectParse("0xffe0.00000001", Float16.greatestFiniteMagnitude)
  expectParse("0xffe8.0", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.0", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.f", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.ff", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.ff8", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.ffc", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.fff", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.fffff", Float16.greatestFiniteMagnitude)
  expectParse("0xffef.fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", Float16.greatestFiniteMagnitude)
  expectParse("0xfff0.0", Float16.infinity) // "Rounds up" by rationale above
  expectParse("0x10000.0", Float16.infinity) // .gFM + epsilon above
}

tests.test("Decimal Floats") {
  expectParseFails(".")
  expectParse("0.0", 0.0)
  expectParse("0", 0.0)
  expectParse("0.", 0.0)
  expectParse(".0", 0.0)
  expectParse("1", 1.0)
  expectParse("2", 2.0)
  expectParse("1e0", 1.0)
  expectParse("3.7e1", 37.0)
  expectParse("12.34e3", 12336.0)
  expectParse("-00.0047e5", -470.0)
  expectParse("2e0", 2.0)
  expectParse("1e1", 10.0)
  expectParse("7e1", 70.0)
  expectParse("1e2", 100.0)
  expectParse("1e3", 1000.0)
  expectParse("1e4", 10000.0)
  expectParse("1e5", Float16.infinity)
  expectParse("1e6", Float16.infinity)
  expectParse("1e7", Float16.infinity)
  expectParse("1e9999999999999999999999999999999999", Float16.infinity)
  expectParse("1e0000000000000000000000000000000001", 10.0)
  expectParse("1", 1.0)
  expectParse("1.0", 1.0)
  expectParse("1.00000000", 1.0)
  expectParse("2.0", 2.0)
  expectParse("0.000001", 1e-6)
  expectParse("0.0000001", 1e-7)
  expectParse("0.00000001", 0.0)
  expectParse("0.000000001", 0.0)
  expectParse("0.0000000001", 0.0)
  expectParse("0.00000000001", 0.0)
  expectParse("0.000000000001", 0.0)
  expectParse("0.0000000000001", 0.0)
  expectParse("0.00000000000001", 0.0)
}

tests.test("Exhaustive Float16") {
  for i in 0..<0xffff {
    let f = Float16(bitPattern: UInt16(i))
    let s = f.debugDescription
    if !f.isNaN {
      expectParse(s, f)
    } else {
      let r = Float16(Substring(s))!
      expectTrue(r.isNaN)
    }
  }
}

runAllTests()
