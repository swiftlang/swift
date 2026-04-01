// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/a.out -enable-experimental-feature Extern
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

// Needed to declare the ABI entry point
// REQUIRES: swift_feature_Extern

// These tests exercise known bugs in older Swift stdlib
// UNSUPPORTED: use_os_stdlib

import StdlibUnittest

let tests = TestSuite("FloatingPointParsing")

fileprivate func expectRoundTrip(
  _ value: Float64,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let text = value.debugDescription
  let roundTrip = Float64(Substring(text))
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
  _ expected: Float64,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let parsed = Float64(Substring(input))
  let msg = "\(input) did not parse to \(expected)"
  expectNotNil(parsed, msg, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  if let parsed {
    expectEqual(parsed.bitPattern, expected.bitPattern, msg, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  }
}

func expectParseFails(
  _ input: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  let parsed = Float64(Substring(input))
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
  expectParse("inf", Float64.infinity)
  expectParse("+inf", Float64.infinity)
  expectParse("-inf", -Float64.infinity)
  expectParse("INF", Float64.infinity)
  expectParse("InF", Float64.infinity)
  expectParse("iNf", Float64.infinity)
  expectParse("infinity", Float64.infinity)
  expectParse("INFINITY", Float64.infinity)
  expectParse("+infinity", Float64.infinity)
  expectParse("-infinity", -Float64.infinity)

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

  expectRoundTrip(Float64.infinity)
  expectRoundTrip(-Float64.infinity)
}

tests.test("NaNs") {
  // Note: Previous Swift runtime used libc strtof and then
  // truncated to Float64, which is why some of these are
  // wrong when testing previous runtimes.
  expectRoundTrip(Float64.nan)
  expectRoundTrip(-Float64.nan)
  expectRoundTrip(Float64(nan:73, signaling:false))
  expectRoundTrip(Float64(nan:73, signaling:true))
  expectParse("nan", Float64.nan)
  expectParse("NAN", Float64.nan)
  expectParse("NaN", Float64.nan)
  expectParse("-nan", -Float64.nan)
  expectParse("nan()", Float64.nan)
  expectParse("nan(0)", Float64.nan)
  expectParse("nan(000000000000000000000000000000000000000)", Float64.nan)
  expectParse("nan(0x00000000000000000000000000000000000000)", Float64.nan)
  expectParse("nan(10)", Float64(nan:10, signaling:false))
  expectParse("nan(1234567890)", Float64(nan:1234567890, signaling:false))
  expectParse("nan(0x10)", Float64(nan:16, signaling:false))
  expectParse("nan(0x12345678)", Float64(nan:0x12345678, signaling:false))
  expectParse("nan(0x9abcdef)", Float64(nan:0x9abcdef, signaling:false))
  expectParse("nan(010)", Float64(nan:8, signaling:false))
  expectParse("nan(01234567)", Float64(nan:0o1234567, signaling:false))
  expectParse("nan(9)", Float64(nan:9, signaling:false))
  expectParse("nan(99)", Float64(nan:99, signaling:false))
  expectParse("nan(255)", Float64(nan:255, signaling:false))
  expectParse("nan(256)", Float64(nan:256, signaling:false))
  expectParse("nan(511)", Float64(nan:511, signaling:false))
  expectParse("nan(999999)", Float64(nan:999999, signaling:false))
  expectParse("nan(999999999999999)", Float64(nan:0x38d7ea4c67fff, signaling:false))
  expectParse("nan(0xfffffffffffff)", Float64(nan:0x3ffffffffffff, signaling:false))
  expectParseFails("n")
  expectParseFails("na")
  expectParseFails("nann")
  expectParseFails("nananananana")
  expectParseFails("nan(xyz)")
  expectParseFails("nan(01238)")
  expectParseFails("nan(01239)")
  expectParseFails("nan(0123a)")
  expectParseFails("nan(0123b)")
  expectParseFails("nan(0123c)")
  expectParseFails("nan(0123d)")
  expectParseFails("nan(0123e)")
  expectParseFails("nan(0123f)")
  expectParseFails("nan(0123g)")
  expectParseFails("nan(123a)")
  expectParseFails("nan(123b)")
  expectParseFails("nan(123c)")
  expectParseFails("nan(123d)")
  expectParseFails("nan(123e)")
  expectParseFails("nan(123f)")
  expectParseFails("nan(123g)")
  expectParseFails("nan(0x123g)")
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
  expectParse("0x.000001", 0x0.000001p0)

  expectParse("0x0.80000000000p-1074", 0.0)
  expectParse("0x0.8000000000000000000000000000000000000000001p-1074", Float64.leastNonzeroMagnitude)
  expectParse("0x0.80000000000000008p-1074", Float64.leastNonzeroMagnitude)
  expectParse("0x0.8000000000000001p-1074", Float64.leastNonzeroMagnitude)
  expectParse("0x0.80000000001p-1074", Float64.leastNonzeroMagnitude)

  expectParse("0x0.0000000000000fffffffffffp-1022", Float64.leastNonzeroMagnitude)
  expectParse("0x0.fffffffffffp-1074", Float64.leastNonzeroMagnitude)

  expectParse("0x1p-1074", Float64.leastNonzeroMagnitude)
  expectParse("0x1p-1074", Float64(bitPattern:1))
  expectParse("0x1p-1073", Float64(bitPattern:2))
  expectParse("0x1p-1072", Float64(bitPattern:4))
  expectParse("0x1p-1071", Float64(bitPattern:8))
  expectParse("0x1p-1070", Float64(bitPattern:16))

  // Test the tricky rounding of values between the largest subnormal and least normal
  expectParse("0x0.fffffffffffffp-1022", Float64.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x1.ffffffffffffep-1023", Float64.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x1.ffffffffffffefffffffffffffffffffffp-1023", Float64.leastNormalMagnitude.nextDown) // Largest subnormal
  expectParse("0x1.fffffffffffffp-1023", Float64.leastNormalMagnitude) // Halfway
  expectParse("0x1.fffffffffffff00000000000000001p-1023", Float64.leastNormalMagnitude) // just above Halfway

  expectParse("0x1p-1022", Float64.leastNormalMagnitude) // Smallest normal

  expectParse("0x1.5555555555555p-2", 1.0/3.0)
  expectParse("0x0.fffffffffffff8", Float64(1.0).nextDown) // Exactly
  expectParse("0x0.fffffffffffffc", 1.0) // Halfway
  expectParse("0x1p0", 1.0)

  expectParse("0x1.00000000000008p0", 1.0) // Halfway, rounds even
  expectParse("0x1.0000000000000800000000000000000000000000000000000000000000001p0", Float64(1.0).nextUp) // Halfway + epsilon
  expectParse("0x1.0000000000001p0", Float64(1.0).nextUp) // Exactly
  expectParse("0x1.000000000000100000000001", Float64(1.0).nextUp) // Bigger than above
  expectParse("0x1000000000000100000000000000001p-120", (Float64(1.0)).nextUp)
  expectParse("0x00000000.0000000000000000000000000000010000000000001000000000000000000001p120", Float64(1.0).nextUp)
  expectParse("0x1p+1", 2.0)
  expectParse("0x1p+0000000000000000000000000000000000001", 2.0)
  expectParse("0x12", 18.0)
  expectParse("0xab", 171.0)
  expectParse("0x1p+10", 1024.0)
  expectParse("0x1p+0000000000000000000000000010", 1024.0)
  expectParse("0x1.921fb54442d18p+1", Float64.pi)

  // Rationale for the four assertions below:
  // * Float64.greatestFiniteMagnitude has an odd significand
  // * Let epsilon = the difference between Float64.greatestFiniteMagnitude and its immediate predecessor
  // * Define a synthetic finite successor to Float64.gFM as Float64.gFM + epsilon
  // * Assertion:  the value above should round to infinity
  // * Assertion:  the value above should be treated as having an even significand
  // * Conclusion:  Exact halfway between Float64.gFM and the value above is the smallest magnitude that should round to infinity
  expectParse("0x1.fffffffffffffp+1023", Float64.greatestFiniteMagnitude) // Exact
  expectParse("0x1.fffffffffffff7fffffffffffffffffffffp+1023", Float64.greatestFiniteMagnitude) // .gFM + less than 1/2 epsilon
  expectParse("0x1.fffffffffffff8p+1023", Float64.infinity) // .gFM + 1/2 epsilon
  expectParse("0x2.0000000000000p+1023", Float64.infinity) // .gFM + epsilon above

  expectParse("0x123456789abcdefp123456789", Float64.infinity)
}

tests.test("Decimal Floats") {
  expectParse("1e-163", 1e-163)
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
  expectParse("0000000000000000.000000000000000e9999999999", 0.0)
  expectParse("0e9999999999", 0.0)
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

  expectParse("5e-324", Float64.leastNonzeroMagnitude)
  // Exact decimal form of 2^-1074 (which is exactly Float64.leastNonzeroMagnitude)
  expectParse("0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004940656458412465441765687928682213723650598026143247644255856825006755072702087518652998363616359923797965646954457177309266567103559397963987747960107818781263007131903114045278458171678489821036887186360569987307230500063874091535649843873124733972731696151400317153853980741262385655911710266585566867681870395603106249319452715914924553293054565444011274801297099995419319894090804165633245247571478690147267801593552386115501348035264934720193790268107107491703332226844753335720832431936092382893458368060106011506169809753078342277318329247904982524730776375927247874656084778203734469699533647017972677717585125660551199131504891101451037862738167250955837389733598993664809941164205702637090279242767544565229087538682506419718265533447265625", Float64.leastNonzeroMagnitude)
  // Exact decimal form of 2^-1075 (halfway between Float64.lNM and 0)
  // Ties round even, so this rounds down to zero
  expectParse("0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000024703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125", 0.0)
  // Increment the last digit, this should round up
  expectParse("0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000024703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328126", Float64.leastNonzeroMagnitude)

  // Even a teeny-tiny bit larger than 2^-1075 should round up
  expectParse("0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002470328229206232720882843964341106861825299013071623822127928412503377536351043759326499181808179961898982823477228588654633283551779698981993873980053909390631503565951557022639229085839244910518443593180284993653615250031937045767824921936562366986365848075700158576926990370631192827955855133292783433840935197801553124659726357957462276646527282722005637400648549997709659947045402082816622623785739345073633900796776193057750674017632467360096895134053553745851666113422376667860416215968046191446729184030053005753084904876539171138659164623952491262365388187963623937328042389101867234849766823508986338858792562830275599565752445550725518931369083625477918694866799496832404970582102851318545139621383772282614543769341253209859132766723632812500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001", Float64.leastNonzeroMagnitude)

  expectParse("2.2250738585072009e-308", Float64.leastNormalMagnitude.nextDown)
  expectParse("2.2250738585072014e-308", Float64.leastNormalMagnitude)
  expectParse("1.7976931348623157e+308", Float64.greatestFiniteMagnitude)
  expectParse("-1.7976931348623157e+308", -Float64.greatestFiniteMagnitude)

  expectParse("179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858367", Float64.greatestFiniteMagnitude) // Exact - 1
  expectParse("179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368", Float64.greatestFiniteMagnitude) // Exact
  expectParse("179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858369", Float64.greatestFiniteMagnitude) // Exact + 1

  // exact gFM + 1 ULP
  expectParse("179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216", Float64.infinity)

  // 1 less than exact midpoint between gFM and gFM + 1 ULP
  // (Largest integer that rounds to gFM)
  expectParse("179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791", Float64.greatestFiniteMagnitude)
  // Even closer to (but still less than) the exact midpoint
  expectParse("179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999", Float64.greatestFiniteMagnitude)

  // Exact midpoint between gFM and gFM + 1 ULP
  // (Rounds even to gFM + 1 ULP, which we treat as infinite
  expectParse("179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497792", Float64.infinity)

  expectParse("1.7976931348623157e+308", Float64.greatestFiniteMagnitude)
  expectParse("1.7976931348623159e+308", Float64.infinity)
  expectParse("1.797693134862316e+308", Float64.infinity)
  expectParse("1.79769313486232e+308", Float64.infinity)
  expectParse("1.79769313487e+308", Float64.infinity)
  expectParse("1.79769314e+308", Float64.infinity)
  expectParse("1.7977e+308", Float64.infinity)
  expectParse("1.798e+308", Float64.infinity)
  expectParse("1.8e+308", Float64.infinity)
  expectParse("2e+308", Float64.infinity)
  expectParse("1e309", Float64.infinity)
  expectParse("1e9999999999999999999999999999999999", Float64.infinity)
  expectParse("999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999.999999999999999999999999999", Float64.infinity)
}

tests.test("Substring - short") {
  let s1 = "1.02.03.0"
  let s1sub = s1[s1.firstIndex(of: "2")!..<s1.firstIndex(of: "3")!]
  let parsed = Float64(s1sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}

tests.test("Substring - long") {
  let s1 = "1.00000000000000000000000000000000002.0000000000000000000000000000000000000000000000000000000003.00000000000000004.0000000000000000"
  let s1sub = s1[s1.firstIndex(of: "2")!..<s1.firstIndex(of: "3")!]
  let parsed = Float64(s1sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}

/*
 // These need Foundation to run, so can't run on Linux?
tests.test("Bridged - short") {
  let s1 = "1.02.03.0"
  let nss1 = NSString(utf8String: s1)!
  let bridged = String(nss1)
  let range = bridged.firstIndex(of: "2")!..<bridged.firstIndex(of: "3")!
  let sub = bridged[range]
  let parsed = Float64(sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}

tests.test("Bridged - long") {
  let s1 = "1.02.0000000000000000000000000000000000000000000000000000000000003.04.05.06.07.08.09.010.011.012.013.014.015.0"
  let nss1 = NSString(utf8String: s1)!
  let bridged = String(nss1)
  let range = bridged.firstIndex(of: "2")!..<bridged.firstIndex(of: "3")!
  let sub = bridged[range]
  let parsed = Float64(sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}
 */

@_extern(c, "_swift_stdlib_strtod_clocale")
func _swift_stdlib_strtod_clocale(
  _: Optional<UnsafePointer<CChar>>,
  _: Optional<UnsafeMutablePointer<Double>>
) -> Optional<UnsafePointer<CChar>>

func viaLegacy(_ text: String) -> Double? {
  return text.withCString { strptr -> Double? in
    var result = Double()
    let succeeded = withUnsafeMutablePointer(to: &result) { dptr in
      let endptr = _swift_stdlib_strtod_clocale(strptr, dptr)
      return endptr == strptr + text.utf8.count
    }
    if succeeded {
      return Double?.some(result)
    } else {
      return Double?.none
    }
  }
}

tests.test("Legacy ABI") {
  expectEqual(viaLegacy("1.0"), 1.0 as Double)
  expectEqual(viaLegacy("1.7976931348623157e+308"), Double.greatestFiniteMagnitude)
}

/*
// Checking 100 million random doubles takes only a fraction of a second on a
// release build, but is _PAINFULLY SLOW_ in debug builds, so only enable this
// locally!
tests.test("Random Float64") {
  let blocks = 100_000
  let blocksize = 1_000
  for _ in 0..<blocks {
    var raw = UInt64.random(in: 0...UInt64.max)
    for _ in 0..<blocksize {
      raw &+= 1
      let d = Float64(bitPattern: raw)
      expectRoundTrip(d)
    }
  }
}
*/

runAllTests()
