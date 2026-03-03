// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/a.out -enable-experimental-feature Extern
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

// Needed to verify the legacy ABI
// REQUIRES: swift_feature_Extern

// Swift earlier than 5.3 fails many of these tests
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest

let tests = TestSuite("FloatingPointParsing")

fileprivate func expectRoundTrip(
  _ value: Float32,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let text = value.debugDescription
  let roundTrip = Float32(Substring(text))
  expectNotNil(roundTrip, text, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  if let roundTrip {
    if value.isNaN {
      // We cannot in general guarantee perfect round-tripping for NaN values,
      // but we can verify that printing/parsing a NaN does result in another
      // NaN.
      expectTrue(roundTrip.isNaN, text, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
    } else {
      expectEqual(roundTrip.bitPattern, value.bitPattern, text, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
    }
  }
}

fileprivate func expectParse(
  _ input: String,
  _ expected: Float32,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let parsed = Float32(Substring(input))
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
  let parsed = Float32(Substring(input))
  expectNil(parsed, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
}

tests.test("Invalids") {
  expectParseFails("")
  expectParseFails(".")
  expectParseFails("e0")
  expectParseFails(".e0")
  expectParseFails("1e+")
  expectParseFails("-")
  expectParseFails("+")
  expectParseFails("&")
  expectParseFails("+x")
  expectParseFails("x")
}

tests.test("Infinities") {
  expectParse("inf", Float32.infinity)
  expectParse("+inf", Float32.infinity)
  expectParse("-inf", -Float32.infinity)
  expectParse("INF", Float32.infinity)
  expectParse("InF", Float32.infinity)
  expectParse("iNf", Float32.infinity)
  expectParse("infinity", Float32.infinity)
  expectParse("INFINITY", Float32.infinity)
  expectParse("+infinity", Float32.infinity)
  expectParse("-infinity", -Float32.infinity)

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

  expectRoundTrip(Float32.infinity)
  expectRoundTrip(-Float32.infinity)
}

tests.test("NaNs") {
  // Note: Previous Swift runtime used libc strtof and then
  // truncated to Float32, which is why some of these are
  // wrong when testing previous runtimes.
  expectRoundTrip(Float32.nan)
  expectRoundTrip(-Float32.nan)
  expectRoundTrip(Float32(nan:73, signaling:false))
  expectRoundTrip(Float32(nan:73, signaling:true))
  expectParse("nan", Float32.nan)
  expectParse("NAN", Float32.nan)
  expectParse("NaN", Float32.nan)
  expectParse("-nan", -Float32.nan)
  expectParse("nan()", Float32.nan)
  expectParse("nan(0)", Float32.nan)
  expectParse("nan(000000000000000000000000000000000000000)", Float32.nan)
  expectParse("nan(0x00000000000000000000000000000000000000)", Float32.nan)
  expectParse("nan(10)", Float32(nan:10, signaling:false))
  expectParse("nan(0x10)", Float32(nan:16, signaling:false))
  expectParse("nan(010)", Float32(nan:8, signaling:false))
  expectParse("nan(9)", Float32(nan:9, signaling:false))
  expectParse("nan(99)", Float32(nan:99, signaling:false))
  expectParse("nan(255)", Float32(nan:255, signaling:false))
  expectParse("nan(256)", Float32(nan:256, signaling:false))
  expectParse("nan(511)", Float32(nan:511, signaling:false))
  expectParse("nan(999999)", Float32(nan:999999, signaling:false))
  expectParse("nan(999999999999999)", Float32(nan:0x67fff, signaling:false))
  expectParse("nan(0xfffffffffffff)", Float32(nan:0x1fffff, signaling:false))
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

  expectParse("0x.000001", 5.9604645e-08)
  expectParse("0x1p-150", 0.0)
  expectParse("0x0.0000010000000000000000000000p-126", 0.0)
  expectParse("0x0.00000100000000000000000000000000000000000000000000000000000000000000000000000000000000000001p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.00000100000000001p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.0000010000000000000001p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.0000010000000000000002p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.0000010000000000000004p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.0000010000000000000008p-126", Float32.leastNonzeroMagnitude)
  expectParse("0x0.000001000000000000001p-126", Float32.leastNonzeroMagnitude)

  expectParse("0x1p-149", Float32.leastNonzeroMagnitude)
  expectParse("0x1p-149", Float32(bitPattern:1))
  expectParse("0x1p-148", Float32(bitPattern:2))
  expectParse("0x1p-147", Float32(bitPattern:4))
  expectParse("0x1p-146", Float32(bitPattern:8))
  expectParse("0x1p-145", Float32(bitPattern:16))

  // Test the tricky rounding of values between the largest subnormal and least normal
  expectParse("0x1.fffffcp-127", Float32.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x0.fffffep-126", Float32.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x0.fffffefffffp-126", Float32.leastNormalMagnitude.nextDown) // Just less than halfway
  expectParse("0x0.ffffffp-126", Float32.leastNormalMagnitude) // Halfway
  expectParse("0x1.fffffep-127", Float32.leastNormalMagnitude) // Halfway
  expectParse("0x0.ffffff0000000001p-126", Float32.leastNormalMagnitude) // Just above halfway
  expectParse("0x1p-126", Float32.leastNormalMagnitude) // Smallest normal

  expectParse("0x1.555556p-2", 1.0/3.0)
  expectParse("0x0.ffffff", Float(1.0).nextDown) // Exactly
  expectParse("0x0.ffffff8", 1.0) // Halfway
  expectParse("0x1p0", 1.0)
  expectParse("0x1.000001p0", 1.0) // Halfway, rounds even
  expectParse("0x1.00000100000000000000000000000000000000000000000000001p0", Float(1.0).nextUp) // Halfway + epsilon
  expectParse("0x1.000002p0", Float(1.0).nextUp) // Exactly
  expectParse("0x1.00000200000000001", Float(1.0).nextUp) // Bigger than above
  expectParse("0x1000002000000000000000000000001p-120", (Float(1.0)).nextUp)
  expectParse("0x00000000.000000000000000000000000000001000002000000000000000000001p120", Float(1.0).nextUp)
  expectParse("0x1p+1", 2.0)
  expectParse("0x1p+0000000000000000000000000000000000001", 2.0)
  expectParse("0x12", 18.0)
  expectParse("0xab", 171.0)
  expectParse("0x1p+10", 1024.0)
  expectParse("0x1p+0000000000000000000000000010", 1024.0)
  expectParse("0x1.921fb4p+1", Float32.pi)

  // Rationale for the four assertions below:
  // * Float32.greatestFiniteMagnitude has an odd significand
  // * Let epsilon = the difference between Float32.greatestFiniteMagnitude and its immediate predecessor
  // * Define a synthetic finite successor to Float32.gFM as Float32.gFM + epsilon
  // * Assertion:  the value above should round to infinity
  // * Assertion:  the value above should be treated as having an even significand
  // * Conclusion:  Exact halfway between Float32.gFM and lIM is the smallest magnitude that should round to infinity
  expectParse("0x1.fffffep+127", Float32.greatestFiniteMagnitude) // Exact
  expectParse("0x1.fffffefffffffffffffffffffffp+127", Float32.greatestFiniteMagnitude) // .gFM + less than 1/2 epsilon
  expectParse("0x1.ffffffp+127", Float32.infinity) // .gFM + 1/2 epsilon
  expectParse("0x2.000000p+127", Float32.infinity) // .gFM + epsilon above

  expectParse("0x123456789abcdefp123456789", Float32.infinity)
}

tests.test("Decimal Floats") {
  expectParse("9007199254740992.0", 9007199254740992.0)
  expectParse("-9007199254740992.0", -9007199254740992.0)
  expectParse("4503599627370496.0", 4503599627370496.0)
  expectParse("7.888609052210118e-31", 7.888609052210118e-31)
  expectParse("3.944304526105059e-31", 3.944304526105059e-31)
  expectParse(".0", 0.0)
  expectParse("0", 0.0)
  expectParse("0.", 0.0)
  expectParse("0.0", 0.0)
  expectParse("000000000000000000000000000000", 0.0)
  expectParse(".000000000000000000000000000000", 0.0)
  expectParse("000000000000000000000000000000.0000000000000000000000000000", 0.0)
  expectParse("1", 1.0)
  expectParse("2", 2.0)
  expectParse("1e0", 1.0)
  expectParse("3.7e1", 37.0)
  expectParse("12.34e3", 12340.0)
  expectParse("-00.0047e5", -470.0)
  expectParse("2e0", 2.0)
  expectParse("1e1", 10.0)
  expectParse("7e1", 70.0)
  expectParse("1e2", 100.0)
  expectParse("1e3", 1000.0)
  expectParse("1e4", 10000.0)
  expectParse("1e0000000000000000000000000000000001", 10.0)
  expectParse("1", 1.0)
  expectParse("1.0", 1.0)
  expectParse("1.00000000", 1.0)
  expectParse("2.0", 2.0)

  expectParse("0.000001", 1e-6)
  expectParse("0.0000001", 1e-7)
  expectParse("0.00000001", 1e-8)
  expectParse("0.000000001", 1e-9)
  expectParse("0.0000000001", 1e-10)
  expectParse("0.00000000001", 1e-11)
  expectParse("0.000000000001", 1e-12)
  expectParse("0.0000000000001", 1e-13)
  expectParse("0.00000000000001", 1e-14)
  expectParse("1e-45", Float32.leastNonzeroMagnitude)
  // Exact decimal form of 2^-149 (which is exactly Float32.leastNonzeroMagnitude)
  expectParse("0.00000000000000000000000000000000000000000000140129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125", Float32.leastNonzeroMagnitude)
  // Exact decimal form of 2^-150 (halfway between Float32.lNM and 0)
  // Ties round even, so this rounds down to zero
  expectParse("0.000000000000000000000000000000000000000000000700649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625", 0.0)
  // Increment the last digit, this should round up
  expectParse("0.000000000000000000000000000000000000000000000700649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015626", Float32.leastNonzeroMagnitude)
  // Tiny tiny tiny bit larger than halfway between Float32.lNM and 0, so rounds up
  expectParse("0.00000000000000000000000000000000000000000000070064923216240853546186479164495806564013097093825788587853414194489554134293030074331909418106079101562500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001", Float32.leastNonzeroMagnitude)
  expectParse("00000000000000000000000000000000000000000000070064923216240853546186479164495806564013097093825788587853414194489554134293030074331909418106079101562500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001.0e-320", Float32.leastNonzeroMagnitude)
  expectParse("11754942e-45", Float32.leastNormalMagnitude.nextDown)
  expectParse("117549432e-46", Float32.leastNormalMagnitude)
  expectParse("34028235e+31", Float32.greatestFiniteMagnitude)
  expectParse("-34028235e+31", -Float32.greatestFiniteMagnitude)
  expectParse("340282346638528859811704183484516925439", Float32.greatestFiniteMagnitude) // Exact - 1
  expectParse("340282346638528859811704183484516925440", Float32.greatestFiniteMagnitude) // Exact
  expectParse("340282346638528859811704183484516925441", Float32.greatestFiniteMagnitude) // Exact + 1
  // 1 less than exact midpoint between gFM and gFM + 1 ULP
  // (Largest integer that rounds to gFM)
  expectParse("340282356779733661637539395458142568447", Float32.greatestFiniteMagnitude)
  expectParse("340282356779733661637539395458142568447.99999999999999999999999999999999999999999999999999999", Float32.greatestFiniteMagnitude)
  // Exact midpoint between gFM and gFM + 1 ULP
  // (Rounds even to gFM + 1 ULP, which we treat as infinite
  expectParse("340282356779733661637539395458142568448", Float32.infinity)
  expectParse("340282356779733661637539395458142568448.000000000000000000000000000000000000000000000000", Float32.infinity)
  expectParse("340282356779733661637539395458142568448.000000000000000000000000000000000000000000000001", Float32.infinity)
  expectParse("3.4028235e+38", Float32.greatestFiniteMagnitude)
  expectParse("3.4028236e+38", Float32.infinity)
  expectParse("3.4028237e+38", Float32.infinity)
  expectParse("3.402824e+38", Float32.infinity)
  expectParse("3.40283e+38", Float32.infinity)
  expectParse("3.4029e+38", Float32.infinity)
  expectParse("3.403e+38", Float32.infinity)
  expectParse("3.41e+38", Float32.infinity)
  expectParse("3.5e+38", Float32.infinity)
  expectParse("4e+38", Float32.infinity)
  expectParse("1e45", Float32.infinity)
  expectParse("1e309", Float32.infinity)
  expectParse("1e9999999999999999999999999999999999", Float32.infinity)
  expectParse("999999999999999999999999999999999999999.999999999999999999999999999", Float32.infinity)
  expectParse("7674047411400702925974988342550565582448.117", Float32.infinity)
}

@_extern(c, "_swift_stdlib_strtof_clocale")
func _swift_stdlib_strtof_clocale(
  _: Optional<UnsafePointer<CChar>>,
  _: Optional<UnsafeMutablePointer<Float32>>
) -> Optional<UnsafePointer<CChar>>

func viaLegacy(_ text: String) -> Float32? {
  return text.withCString { strptr -> Float32? in
    var result = Float32()
    let succeeded = withUnsafeMutablePointer(to: &result) { dptr in
      let endptr = _swift_stdlib_strtof_clocale(strptr, dptr)
      return endptr == strptr + text.utf8.count
    }
    if succeeded {
      return Float32?.some(result)
    } else {
      return Float32?.none
    }
  }
}

tests.test("Legacy ABI") {
  expectEqual(viaLegacy("1.0"), 1.0 as Float32)
  expectEqual(viaLegacy("3.4028235e+38"), Float32.greatestFiniteMagnitude)
}


/*
// Verify round-trip correctness for every Float32 value.
// Only enable this locally!
tests.test("Exhaustive Float32") {
  for i in 0..<0xffffffff {
    let f = Float32(bitPattern: UInt32(i))
    expectRoundTrip(f)
  }
  expectRoundTrip(Float32(bitPattern: 0xffffffff)
}
*/

/*
// Checking 100 million random floats takes only a fraction of a second on a
// release build, but is _PAINFULLY SLOW_ in debug builds, so only enable this
// locally!
tests.test("Random Float32") {
  let blocks = 100_000
  let blocksize = 1_000
  for _ in 0..<blocks {
    var raw = UInt32.random(in: 0...UInt32.max)
    for _ in 0..<blocksize {
      raw &+= 1
      let d = Float32(bitPattern: raw)
      expectRoundTrip(d)
    }
  }
}
*/

runAllTests()
