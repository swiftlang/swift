// RUN: rm -rf %t && mkdir -p %t && %S/../../utils/gyb %s -o %t/NSStringAPI.swift
// RUN: %S/../../utils/line-directive %t/NSStringAPI.swift -- %target-build-swift -module-cache-path %t/clang-module-cache %t/NSStringAPI.swift -o %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/NSStringAPI_test.txt | %S/../../utils/line-directive %t/NSStringAPI.swift -- FileCheck %s

//
// Tests for the NSString APIs as exposed by String
//

import Foundation

var _anyExpectFailed = false

// Can not write a sane set of overloads using generics because of:
// <rdar://problem/17015923> Array->NSArray implicit conversion insanity
%for EquatableArrayType in ['ContiguousArray', 'Slice', 'Array']:

func expectEqual<T : Equatable>(
    expected: ${EquatableArrayType}<T>, actual: ${EquatableArrayType}<T>,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if expected != actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    println()
  }
}

func expectNotEqual<T : Equatable>(
    expected: ${EquatableArrayType}<T>, actual: ${EquatableArrayType}<T>,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if expected == actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("unexpected value: \"\(actual)\"")
    println()
  }
}

%end

%for (Generic, EquatableType) in [('', 'String'), ('', 'String.Index'), ('', 'Int'), ('', 'UInt'), ('', 'NSComparisonResult'), ('<T : ForwardIndex>', 'T'), ('<T, U : Equatable>', 'Dictionary<T, U>')]:

func expectEqual${Generic}(
    expected: ${EquatableType}, actual: ${EquatableType},
    file: String = __FILE__, line: UWord = __LINE__
) {
  if expected != actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    println()
  }
}

func expectEqual${Generic}(
    expected: ${EquatableType}, actual: ${EquatableType}?,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if !actual || expected != actual! {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    println()
  }
}

func expectNotEqual${Generic}(
    expected: ${EquatableType}, actual: ${EquatableType},
    file: String = __FILE__, line: UWord = __LINE__
) {
  if expected == actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("unexpected value: \"\(actual)\"")
    println()
  }
}

%end

%for ComparableType in ['Int']:

func expectLE(
    expected: ${ComparableType}, actual: ${ComparableType},
    file: String = __FILE__, line: UWord = __LINE__
) {
  if !(expected <= actual) {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    println()
  }
}

%end

func expectTrue(
    actual: Bool,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if !actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: true")
    println("actual: \(actual)")
    println()
  }
}

func expectFalse(
    actual: Bool,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected: false")
    println("actual: \(actual)")
    println()
  }
}

func expectEmpty<T>(
    value: Optional<T>,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if value {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected optional to be empty")
    println("actual: \"\(value)\"")
    println()
  }
}

func expectNotEmpty<T>(
    value: Optional<T>,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if !value {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("expected optional to be non-empty")
    println()
  }
}

struct TestCase {
  init(_ name: String) {
    self.name = name
  }

  mutating func test(name: String, testFunction: () -> ()) {
    _tests.append(_Test(name: name, code: testFunction))
  }

  mutating func run() {
    var anyTestFailed = false
    for t in _tests {
      var fullTestName = "\(name).\(t.name)"
      println("[ RUN      ] \(fullTestName)")
      _anyExpectFailed = false
      t.code()
      if _anyExpectFailed {
        anyTestFailed = true
        println("[     FAIL ] \(fullTestName)")
      } else {
        println("[       OK ] \(fullTestName)")
      }
    }
    if anyTestFailed {
      println("Some tests failed, aborting")
      abort()
    } else {
      println("\(name): All tests passed")
    }
  }

  struct _Test {
    var name: String
    var code: () -> ()
  }

  var name: String
  var _tests: _Test[] = []
}

var NSStringAPIs = TestCase("NSStringAPIs")

NSStringAPIs.test("Encodings") {
  let availableEncodings: NSStringEncoding[] = String.availableStringEncodings()
  expectNotEqual(0, availableEncodings.count)

  let defaultCStringEncoding = String.defaultCStringEncoding()
  expectTrue(contains(availableEncodings, defaultCStringEncoding))

  expectNotEqual("", String.localizedNameOfStringEncoding(NSUTF8StringEncoding))
}

NSStringAPIs.test("NSStringEncoding") {
  // Make sure NSStringEncoding and its values are type-compatible.
  var enc: NSStringEncoding
  enc = NSWindowsCP1250StringEncoding
  enc = NSUTF32LittleEndianStringEncoding
  enc = NSUTF32BigEndianStringEncoding
  enc = NSASCIIStringEncoding
  enc = NSUTF8StringEncoding
}

NSStringAPIs.test("localizedStringWithFormat(_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String.localizedStringWithFormat(
      "Hello, %@!%%%ld", world, 42))
}

NSStringAPIs.test("pathWithComponents(_:)") {
  expectEqual("flugelhorn/baritone/bass",
      String.pathWithComponents(["flugelhorn", "baritone", "bass"]))
}

var existingPath = Process.arguments[1]
var nonExistentPath = existingPath + "-NoNeXiStEnT"

NSStringAPIs.test("stringWithContentsOfFile(_:encoding:error:)") {
  if true {
    var err: NSError?
    var content = String.stringWithContentsOfFile(existingPath,
        encoding: NSASCIIStringEncoding, error: &err)

    expectEmpty(err)
    expectEqual("Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
        content?._lines[0])
  }
  if true {
    var err: NSError?
    var content = String.stringWithContentsOfFile(nonExistentPath,
        encoding: NSASCIIStringEncoding, error: &err)

    expectNotEmpty(err)
    expectEmpty(content)
  }
}

NSStringAPIs.test("stringWithContentsOfFile(_:usedEncoding:error:)") {
  if true {
    var usedEncoding: NSStringEncoding = 0
    var err: NSError?
    var content = String.stringWithContentsOfFile(existingPath,
        usedEncoding: &usedEncoding, error: &err)

    expectNotEqual(0, usedEncoding)
    expectEmpty(err)
    expectEqual("Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
        content?._lines[0])
  }
  if true {
    var usedEncoding: NSStringEncoding = 0
    var err: NSError?
    var content = String.stringWithContentsOfFile(nonExistentPath, error: &err)

    expectEqual(0, usedEncoding)
    expectNotEmpty(err)
    expectEmpty(content)
  }
}

var existingURL = NSURL.URLWithString("file://" + existingPath)
var nonExistentURL = NSURL.URLWithString("file://" + nonExistentPath)

NSStringAPIs.test("stringWithContentsOfURL(_:encoding:error:)") {
  if true {
    var err: NSError?
    var content = String.stringWithContentsOfURL(existingURL,
        encoding: NSASCIIStringEncoding, error: &err)

    expectEmpty(err)
    expectEqual("Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
        content?._lines[0])
  }
  if true {
    var err: NSError?
    var content = String.stringWithContentsOfURL(nonExistentURL,
        encoding: NSASCIIStringEncoding, error: &err)

    expectNotEmpty(err)
    expectEmpty(content)
  }
}

NSStringAPIs.test("stringWithContentsOfURL(_:usedEncoding:error:)") {
  if true {
    var usedEncoding: NSStringEncoding = 0
    var err: NSError?
    var content = String.stringWithContentsOfURL(existingURL,
        usedEncoding: &usedEncoding, error: &err)

    expectNotEqual(0, usedEncoding)
    expectEmpty(err)
    expectEqual("Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
        content?._lines[0])
  }
  if true {
    var usedEncoding: NSStringEncoding = 0
    var err: NSError?
    var content = String.stringWithContentsOfURL(nonExistentURL,
        usedEncoding: &usedEncoding, error: &err)

    expectEqual(0, usedEncoding)
    expectNotEmpty(err)
    expectEmpty(content)
  }
}

NSStringAPIs.test("stringWithCString(_:encoding:)") {
  expectEqual("foo, a basmati bar!",
      String.stringWithCString(
          "foo, a basmati bar!", encoding: String.defaultCStringEncoding()))
}

NSStringAPIs.test("stringWithUTF8String(_:)") {
  var s = "foo あいう"
  var up = UnsafePointer<UInt8>.alloc(100)
  var i = 0
  for b in s.utf8 {
    up[i] = b
    i++
  }
  expectEqual(s, String.stringWithUTF8String(CString(up)))
  up.dealloc(100)
}

NSStringAPIs.test("canBeConvertedToEncoding(_:)") {
  expectTrue("foo".canBeConvertedToEncoding(NSASCIIStringEncoding))
  expectFalse("あいう".canBeConvertedToEncoding(NSASCIIStringEncoding))
}

NSStringAPIs.test("capitalizedString") {
  expectEqual("Foo Foo Foo Foo", "foo Foo fOO FOO".capitalizedString)
  expectEqual("Жжж", "жжж".capitalizedString)
}

NSStringAPIs.test("capitalizedStringWithLocale(_:)") {
  expectEqual("Foo Foo Foo Foo",
      "foo Foo fOO FOO".capitalizedStringWithLocale(NSLocale.currentLocale()))
  expectEqual("Жжж",
      "жжж".capitalizedStringWithLocale(NSLocale.currentLocale()))

  expectEqual("Foo Foo Foo Foo",
      "foo Foo fOO FOO".capitalizedStringWithLocale(nil))
  expectEqual("Жжж", "жжж".capitalizedStringWithLocale(nil))
}

NSStringAPIs.test("caseInsensitiveCompare(_:)") {
  expectEqual(NSComparisonResult.OrderedSame,
      "abCD".caseInsensitiveCompare("AbCd"))
  expectEqual(NSComparisonResult.OrderedAscending,
      "abCD".caseInsensitiveCompare("AbCdE"))

  expectEqual(NSComparisonResult.OrderedSame,
      "абвг".caseInsensitiveCompare("АбВг"))
  expectEqual(NSComparisonResult.OrderedAscending,
      "абВГ".caseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("commonPrefixWithString(_:options:)") {
  expectEqual("ab",
      "abcd".commonPrefixWithString("abdc", options: NSStringCompareOptions(0)))
  expectEqual("abC",
      "abCd".commonPrefixWithString("abce", options: .CaseInsensitiveSearch))

  expectEqual("аб",
      "абвг".commonPrefixWithString("абгв", options: NSStringCompareOptions(0)))
  expectEqual("абВ",
      "абВг".commonPrefixWithString("абвд", options: .CaseInsensitiveSearch))
}

NSStringAPIs.test("compare(_:options:range:locale:)") {
  expectEqual(NSComparisonResult.OrderedSame,
      "abc".compare("abc"))
  expectEqual(NSComparisonResult.OrderedAscending,
      "абв".compare("где"))

  expectEqual(NSComparisonResult.OrderedSame,
      "abc".compare("abC", options: .CaseInsensitiveSearch))
  expectEqual(NSComparisonResult.OrderedSame,
      "абв".compare("абВ", options: .CaseInsensitiveSearch))

  if true {
    let s = "abcd"
    let r = s.startIndex.succ()..s.endIndex
    expectEqual(NSComparisonResult.OrderedSame,
        s.compare("bcd", range: r))
  }
  if true {
    let s = "абвг"
    let r = s.startIndex.succ()..s.endIndex
    expectEqual(NSComparisonResult.OrderedSame,
        s.compare("бвг", range: r))
  }

  expectEqual(NSComparisonResult.OrderedSame,
      "abc".compare("abc", locale: NSLocale.currentLocale()))
  expectEqual(NSComparisonResult.OrderedSame,
      "абв".compare("абв", locale: NSLocale.currentLocale()))
}

NSStringAPIs.test("completePathIntoString(_:caseSensitive:matchesIntoArray:filterTypes)") {
  if true {
    var count = nonExistentPath.completePathIntoString(caseSensitive: false)
    expectEqual(0, count)
  }

  if true {
    var outputName = "None Found"
    var count = nonExistentPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
  }

  if true {
    var outputName = "None Found"
    var outputArray: String[] = [ "foo", "bar" ]
    var count = nonExistentPath.completePathIntoString(
        &outputName, caseSensitive: false, matchesIntoArray: &outputArray)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
    expectEqual([ "foo", "bar" ], outputArray)
  }

  if true {
    var count = existingPath.completePathIntoString(caseSensitive: false)
    expectEqual(1, count)
  }

  if true {
    var outputName = "None Found"
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }

  if true {
    var outputName = "None Found"
    var outputArray: String[] = [ "foo", "bar" ]
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false, matchesIntoArray: &outputArray)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
    expectEqual([ existingPath ], outputArray)
  }

  if true {
    var outputName = "None Found"
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false, filterTypes: [ "txt" ])

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }
}

NSStringAPIs.test("componentsSeparatedByCharactersInSet(_:)") {
  expectEqual([ "абв", "", "あいう", "abc" ],
      "абв12あいう3abc".componentsSeparatedByCharactersInSet(
          NSCharacterSet.decimalDigitCharacterSet()))
}

NSStringAPIs.test("componentsSeparatedByString(_:)") {
  expectEqual([ "абв", "あいう", "abc" ],
      "абв//あいう//abc".componentsSeparatedByString("//"))
}

NSStringAPIs.test("cStringUsingEncoding(_:)") {
  expectEmpty("абв".cStringUsingEncoding(NSASCIIStringEncoding))

  let expectedBytes: UInt8[] = [ 0xd0, 0xb0, 0xd0, 0xb1, 0xd0, 0xb2, 0 ]
  var expectedStr: CChar[] = expectedBytes.map { $0.asSigned() }
  expectEqual(expectedStr,
      "абв".cStringUsingEncoding(NSUTF8StringEncoding)!)
}

NSStringAPIs.test("dataUsingEncoding(_:allowLossyConversion:)") {
  expectEmpty("あいう".dataUsingEncoding(NSASCIIStringEncoding, allowLossyConversion: false))

  if true {
    let data = "あいう".dataUsingEncoding(NSUTF8StringEncoding)
    let bytes = Array(UnsafeArray(
        start: UnsafePointer<UInt8>(data!.bytes), length: data!.length))
    let expectedBytes: UInt8[] = [
      0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86
    ]
    expectTrue(equal(expectedBytes, bytes))
  }
}

NSStringAPIs.test("decomposedStringWithCanonicalMapping") {
  expectEqual("abc", "abc".decomposedStringWithCanonicalMapping)
  expectEqual("\u305f\u3099くてん", "だくてん".decomposedStringWithCanonicalMapping)
  expectEqual("\uff80\uff9eｸﾃﾝ", "ﾀﾞｸﾃﾝ".decomposedStringWithCanonicalMapping)
}

NSStringAPIs.test("decomposedStringWithCompatibilityMapping") {
  expectEqual("abc", "abc".decomposedStringWithCompatibilityMapping)
  expectEqual("\u30bf\u3099クテン", "ﾀﾞｸﾃﾝ".decomposedStringWithCompatibilityMapping)
}

NSStringAPIs.test("enumerateLines(_:)") {
  var lines: String[] = []
  "abc\n\ndefghi\njklm".enumerateLines {
    (line: String, inout stop: Bool)
  in
    lines += line
    if lines.count == 3 {
      stop = true
    }
  }
  expectEqual([ "abc", "", "defghi" ], lines)
}

NSStringAPIs.test("enumerateLinguisticTagsInRange(_:scheme:options:orthography:_:") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = advance(s.startIndex, 5)
  let endIndex = advance(s.startIndex, 62)
  var tags: String[] = []
  var tokens: String[] = []
  var sentences: String[] = []
  s.enumerateLinguisticTagsInRange(startIndex..endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: NSLinguisticTaggerOptions(0),
      orthography: nil) {
    (tag: String, tokenRange: Range<String.Index>, sentenceRange: Range<String.Index>, inout stop: Bool)
  in
    tags += tag
    tokens += s[tokenRange]
    sentences += s[sentenceRange]
    if tags.count == 3 {
      stop = true
    }
  }
  expectEqual(
      [ NSLinguisticTagWord, NSLinguisticTagWhitespace,
        NSLinguisticTagWord ],
      tags)
  expectEqual([ "Глокая", " ", "куздра" ], tokens)
  let sentence = s[startIndex..endIndex]
  expectEqual([ sentence, sentence, sentence ], sentences)
}

NSStringAPIs.test("enumerateSubstringsInRange(_:options:_:)") {
  let s = "え\u304b\u3099お\u263a\ufe0f😀😊"
  let startIndex = advance(s.startIndex, 1)
  // FIXME: this might need to be adjusted to 5 when we implement
  // grapheme clusters properly.
  let endIndex = advance(s.startIndex, 7)
  var substrings: String[] = []
  s.enumerateSubstringsInRange(startIndex..endIndex,
      options: NSStringEnumerationOptions.ByComposedCharacterSequences) {
    (substring: String, substringRange: Range<String.Index>,
     enclosingRange: Range<String.Index>, inout stop: Bool)
  in
    substrings += substring
    expectEqual(substring, s[substringRange])
    expectEqual(substring, s[enclosingRange])
  }
  expectEqual([ "\u304b\u3099", "お", "☺️", "😀" ], substrings)
}

NSStringAPIs.test("fastestEncoding") {
  let availableEncodings: NSStringEncoding[] = String.availableStringEncodings()
  expectTrue(contains(availableEncodings, "abc".fastestEncoding))
}

NSStringAPIs.test("fileSystemRepresentation()") {
  if true {
    let expectedStr: CChar[] = Array(map("abc\0".utf8) { $0.asSigned() })
    expectEqual(expectedStr, "abc".fileSystemRepresentation())
  }

  // On OSX file system representation is Unicode NFD.
  // This test might need to be adjusted for other systems.
  if true {
    let expectedStr: CChar[] =
        Array(map("\u305f\u3099くてん\0".utf8) { $0.asSigned() })
    expectEqual(expectedStr, "だくてん".fileSystemRepresentation())
  }
}

NSStringAPIs.test("getBytes(_:maxLength:usedLength:encoding:options:range:remainingRange:)") {
  let s = "abc абв def где gh жз zzz"
  let startIndex = advance(s.startIndex, 8)
  let endIndex = advance(s.startIndex, 22)
  if true {
    // 'maxLength' is limiting.
    let bufferLength = 100
    var expectedStr: UInt8[] = Array("def где ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr += 0xff
    }
    var buffer = UInt8[](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(11, usedLength)
    expectEqual(remainingRange.startIndex, advance(startIndex, 8))
    expectEqual(remainingRange.endIndex, endIndex)
  }
  if true {
    // 'bufferLength' is limiting.  Note that the buffer is not filled
    // completely, since doing that would break a UTF sequence.
    let bufferLength = 5
    var expectedStr: UInt8[] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr += 0xff
    }
    var buffer = UInt8[](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(4, usedLength)
    expectEqual(remainingRange.startIndex, advance(startIndex, 4))
    expectEqual(remainingRange.endIndex, endIndex)
  }
  if true {
    // 'range' is converted completely.
    let bufferLength = 100
    var expectedStr: UInt8[] = Array("def где gh жз ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr += 0xff
    }
    var buffer = UInt8[](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(19, usedLength)
    expectEqual(remainingRange.startIndex, endIndex)
    expectEqual(remainingRange.endIndex, endIndex)
  }
  if true {
    // Inappropriate encoding.
    let bufferLength = 100
    var expectedStr: UInt8[] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr += 0xff
    }
    var buffer = UInt8[](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: NSASCIIStringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(4, usedLength)
    expectEqual(remainingRange.startIndex, advance(startIndex, 4))
    expectEqual(remainingRange.endIndex, endIndex)
  }
}

NSStringAPIs.test("getCString(_:maxLength:encoding:)") {
  var s = "abc あかさた"
  if true {
    let bufferLength = 16
    var buffer = CChar[](count: bufferLength, repeatedValue: (0xff).asSigned())
    var result = s.getCString(&buffer, maxLength: 100,
        encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
  if true {
    let bufferLength = 17
    var expectedStr: CChar[] = Array(map("abc あかさた\0".utf8) { $0.asSigned() })
    while (expectedStr.count != bufferLength) {
      expectedStr += (0xff).asSigned()
    }
    var buffer = CChar[](count: bufferLength, repeatedValue: (0xff).asSigned())
    var result = s.getCString(&buffer, maxLength: 100,
        encoding: NSUTF8StringEncoding)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
  }
  if true {
    let bufferLength = 100
    var buffer = CChar[](count: bufferLength, repeatedValue: (0xff).asSigned())
    var result = s.getCString(&buffer, maxLength: 8,
        encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
}

NSStringAPIs.test("getFileSystemRepresentation(_:maxLength:)") {
  // On OSX file system representation is Unicode NFD.
  // This test might need to be adjusted for other systems.
  var s = "abc だくてん"
  if true {
    let bufferLength = 16
    var buffer = CChar[](count: bufferLength, repeatedValue: (0xff).asSigned())
    var result = s.getFileSystemRepresentation(&buffer, maxLength: 100)
    expectFalse(result)
  }
  if true {
    let bufferLength = 100
    var expectedStr: CChar[] =
        Array(map("abc \u305f\u3099くてん\0".utf8) { $0.asSigned() })
    while (expectedStr.count != bufferLength) {
      expectedStr += (0xff).asSigned()
    }
    var buffer = CChar[](count: bufferLength, repeatedValue: (0xff).asSigned())
    expectTrue(s.getFileSystemRepresentation(&buffer, maxLength: bufferLength))
    expectTrue(equal(expectedStr, buffer))
  }
}

NSStringAPIs.test("getLineStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = advance(s.startIndex, 16)..advance(s.startIndex, 35)
  if true {
    var outStartIndex = s.startIndex
    var outLineEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getLineStart(&outStartIndex, end: &outLineEndIndex,
        contentsEnd: &outContentsEndIndex, forRange: r)
    expectEqual("штеко будланула\nбокра и кудрячит\n",
        s[outStartIndex..outLineEndIndex])
    expectEqual("штеко будланула\nбокра и кудрячит",
        s[outStartIndex..outContentsEndIndex])
  }
}

NSStringAPIs.test("getParagraphStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\u2028бокра и кудрячит\u2028бокрёнка.\n Абв."
  let r = advance(s.startIndex, 16)..advance(s.startIndex, 35)
  if true {
    var outStartIndex = s.startIndex
    var outEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getParagraphStart(&outStartIndex, end: &outEndIndex,
        contentsEnd: &outContentsEndIndex, forRange: r)
    expectEqual("штеко будланула\u2028бокра и кудрячит\u2028бокрёнка.\n",
        s[outStartIndex..outEndIndex])
    expectEqual("штеко будланула\u2028бокра и кудрячит\u2028бокрёнка.",
        s[outStartIndex..outContentsEndIndex])
  }
}

NSStringAPIs.test("hash") {
  var s: String = "abc"
  var nsstr: NSString = "abc"
  expectEqual(nsstr.hash, s.hash)
}

NSStringAPIs.test("stringWithBytes(_:length:encoding:)") {
  var s: String = "abc あかさた"
  var bytes: UInt8[] = Array(s.utf8)
  expectEqual(s, String.stringWithBytes(bytes, length: bytes.count,
      encoding: NSUTF8StringEncoding))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectEmpty(String.stringWithBytes(bytes, length: bytes.count,
      encoding: NSASCIIStringEncoding))
  */

  // FIXME: add a test where this function actually returns nil.
}

NSStringAPIs.test("stringWithBytesNoCopy(_:length:encoding:freeWhenDone:)") {
  var s: String = "abc あかさた"
  var bytes: UInt8[] = Array(s.utf8)
  expectEqual(s, String.stringWithBytesNoCopy(&bytes, length: bytes.count,
      encoding: NSUTF8StringEncoding, freeWhenDone: false))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectEmpty(String.stringWithBytesNoCopy(&bytes, length: bytes.count,
      encoding: NSASCIIStringEncoding, freeWhenDone: false))
  */

  // FIXME: add a test where this function actually returns nil.
}

NSStringAPIs.test("init(utf16CodeUnits:count:)") {
  let expected = "abc абв \U0001F60A"
  let chars: unichar[] = Array(expected.utf16)

  expectEqual(expected, String(utf16CodeUnits: chars, count: chars.count))
}

NSStringAPIs.test("init(utf16CodeUnitsNoCopy:count:freeWhenDone:)") {
  let expected = "abc абв \U0001F60A"
  let chars: unichar[] = Array(expected.utf16)

  expectEqual(expected, String(utf16CodeUnitsNoCopy: chars,
      count: chars.count, freeWhenDone: false))
}

NSStringAPIs.test("init(format:_:...)") {
  let world: NSString = "world"
  expectEqual("Hello, world!%42",
      String(format: "Hello, %@!%%%ld", world, 42))
}

NSStringAPIs.test("init(format:arguments:)") {
  let world: NSString = "world"
  let args: CVarArg[] = [ world, 42 ]
  expectEqual("Hello, world!%42",
      String(format: "Hello, %@!%%%ld", arguments: args))
}

NSStringAPIs.test("init(format:locale:_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, world, 42))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.systemLocale(), world, 42))
}

NSStringAPIs.test("init(format:locale:arguments:)") {
  let world: NSString = "world"
  let args: CVarArg[] = [ world, 42 ]
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, arguments: args))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.systemLocale(), arguments: args))
}

NSStringAPIs.test("lastPathComponent") {
  expectEqual("bar", "/foo/bar".lastPathComponent)
  expectEqual("абв", "/foo/абв".lastPathComponent)
}

NSStringAPIs.test("utf16count") {
  expectEqual(1, "a".utf16count)
  expectEqual(2, "\U0001F60A".utf16count)
}

NSStringAPIs.test("lengthOfBytesUsingEncoding(_:)") {
  expectEqual(1, "a".lengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  expectEqual(2, "あ".lengthOfBytesUsingEncoding(NSShiftJISStringEncoding))
}

NSStringAPIs.test("lineRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = advance(s.startIndex, 16)..advance(s.startIndex, 35)
  if true {
    let result = s.lineRangeForRange(r)
    expectEqual("штеко будланула\nбокра и кудрячит\n", s[result])
  }
}

NSStringAPIs.test("linguisticTagsInRange(_:scheme:options:orthography:tokenRanges:)") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = advance(s.startIndex, 5)
  let endIndex = advance(s.startIndex, 17)
  var tokenRanges: Range<String.Index>[] = []
  var tags = s.linguisticTagsInRange(startIndex..endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: NSLinguisticTaggerOptions(0),
      orthography: nil, tokenRanges: &tokenRanges)
  expectEqual(
      [ NSLinguisticTagWord, NSLinguisticTagWhitespace,
        NSLinguisticTagWord ],
      tags)
  expectEqual([ "Глокая", " ", "куздра" ],
      tokenRanges.map() { s[$0] } )
}

NSStringAPIs.test("localizedCaseInsensitiveCompare(_:)") {
  expectEqual(NSComparisonResult.OrderedSame,
      "abCD".localizedCaseInsensitiveCompare("AbCd"))
  expectEqual(NSComparisonResult.OrderedAscending,
      "abCD".localizedCaseInsensitiveCompare("AbCdE"))

  expectEqual(NSComparisonResult.OrderedSame,
      "абвг".localizedCaseInsensitiveCompare("АбВг"))
  expectEqual(NSComparisonResult.OrderedAscending,
      "абВГ".localizedCaseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("localizedCompare(_:)") {
  expectEqual(NSComparisonResult.OrderedAscending,
      "abCD".localizedCompare("AbCd"))

  expectEqual(NSComparisonResult.OrderedAscending,
      "абвг".localizedCompare("АбВг"))
}

NSStringAPIs.test("localizedStandardCompare(_:)") {
  expectEqual(NSComparisonResult.OrderedAscending,
      "abCD".localizedStandardCompare("AbCd"))

  expectEqual(NSComparisonResult.OrderedAscending,
      "абвг".localizedStandardCompare("АбВг"))
}

NSStringAPIs.test("lowercaseStringWithLocale(_:)") {
  expectEqual("abcd", "abCD".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  expectEqual("абвг", "абВГ".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))
}

NSStringAPIs.test("maximumLengthOfBytesUsingEncoding(_:)") {
  if true {
    let s = "abc"
    expectLE(countElements(s.utf8),
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
  if true {
    let s = "abc абв"
    expectLE(countElements(s.utf8),
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
  if true {
    let s = "\U0001F60A"
    expectLE(countElements(s.utf8),
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
}

NSStringAPIs.test("paragraphRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\u2028бокра и кудрячит\u2028бокрёнка.\n Абв."
  let r = advance(s.startIndex, 16)..advance(s.startIndex, 35)
  if true {
    let result = s.paragraphRangeForRange(r)
    expectEqual("штеко будланула\u2028бокра и кудрячит\u2028бокрёнка.\n", s[result])
  }
}

NSStringAPIs.test("pathComponents") {
  expectEqual([ "/", "foo", "bar" ], "/foo/bar".pathComponents)
  expectEqual([ "/", "абв", "где" ], "/абв/где".pathComponents)
}

NSStringAPIs.test("pathExtension") {
  expectEqual("", "/foo/bar".pathExtension)
  expectEqual("txt", "/foo/bar.txt".pathExtension)
}

NSStringAPIs.test("precomposedStringWithCanonicalMapping") {
  expectEqual("abc", "abc".precomposedStringWithCanonicalMapping)
  expectEqual("だくてん",
      "\u305f\u3099くてん".precomposedStringWithCanonicalMapping)
  expectEqual("ﾀﾞｸﾃﾝ",
      "\uff80\uff9eｸﾃﾝ".precomposedStringWithCanonicalMapping)
  expectEqual("\ufb03", "\ufb03".precomposedStringWithCanonicalMapping)
}

NSStringAPIs.test("precomposedStringWithCompatibilityMapping") {
  expectEqual("abc", "abc".precomposedStringWithCompatibilityMapping)
  /*
  Test disabled because of:
  <rdar://problem/17041347> NFKD normalization as implemented by
  'precomposedStringWithCompatibilityMapping:' is not idempotent

  expectEqual("\u30c0クテン",
      "\uff80\uff9eｸﾃﾝ".precomposedStringWithCompatibilityMapping)
  */
  expectEqual("ffi", "\ufb03".precomposedStringWithCompatibilityMapping)
}

NSStringAPIs.test("propertyList()") {
  expectEqual([ "foo", "bar" ],
      "(\"foo\", \"bar\")".propertyList() as String[])
}

NSStringAPIs.test("propertyListFromStringsFileFormat()") {
  expectEqual([ "foo": "bar", "baz": "baz" ],
      "/* comment */\n\"foo\" = \"bar\";\n\"baz\";"
          .propertyListFromStringsFileFormat() as Dictionary<String, String>)
}

NSStringAPIs.test("rangeOfCharacterFromSet(_:options:range:)") {
  if true {
    let charset = NSCharacterSet(charactersInString: "абв")
    if true {
      let s = "Глокая куздра"
      let r = s.rangeOfCharacterFromSet(charset)!
      expectEqual(advance(s.startIndex, 4), r.startIndex)
      expectEqual(advance(s.startIndex, 5), r.endIndex)
    }
    if true {
      expectEmpty("клмн".rangeOfCharacterFromSet(charset))
    }
  }

  if true {
    let charset = NSCharacterSet(charactersInString: "\u305f\u3099")
    expectEmpty("\u3060".rangeOfCharacterFromSet(charset))
  }
  if true {
    let charset = NSCharacterSet(charactersInString: "\u3060")
    expectEmpty("\u305f\u3099".rangeOfCharacterFromSet(charset))
  }

  if true {
    let charset = NSCharacterSet(charactersInString: "\U0001F600")
    if true {
      let s = "abc\U0001F600"
      expectEqual("\U0001F600",
          s[s.rangeOfCharacterFromSet(charset)!])
    }
    if true {
      expectEmpty("abc\U0001F601".rangeOfCharacterFromSet(charset))
    }
  }
}

NSStringAPIs.test("rangeOfComposedCharacterSequenceAtIndex(_:)") {
  // FIXME
}

NSStringAPIs.test("rangeOfComposedCharacterSequencesForRange(_:)") {
  // FIXME
}

NSStringAPIs.test("rangeOfString(_:options:range:locale:)") {
  // FIXME
}

NSStringAPIs.test("smallestEncoding") {
  // FIXME
}

NSStringAPIs.test("stringByAbbreviatingWithTildeInPath()") {
  // FIXME
}

NSStringAPIs.test("stringByAddingPercentEncodingWithAllowedCharacters(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByAddingPercentEscapesUsingEncoding(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByAppendingFormat(_:_:...)") {
  // FIXME
}

NSStringAPIs.test("stringByAppendingPathComponent(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByAppendingPathExtension(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByAppendingString(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByDeletingLastPathComponent") {
  // FIXME
}

NSStringAPIs.test("stringByDeletingPathExtension") {
  // FIXME
}

NSStringAPIs.test("stringByExpandingTildeInPath") {
  // FIXME
}

NSStringAPIs.test("stringByFoldingWithOptions(_:locale:)") {
  // FIXME
}

NSStringAPIs.test("stringByPaddingToLength(_:withString:startingAtIndex:)") {
  // FIXME
}

NSStringAPIs.test("stringByRemovingPercentEncoding") {
  // FIXME
}

NSStringAPIs.test("stringByReplacingCharactersInRange(_:withString:)") {
  // FIXME
}

NSStringAPIs.test("stringByReplacingOccurrencesOfString(_:withString:options:range:)") {
  // FIXME
}

NSStringAPIs.test("stringByReplacingPercentEscapesUsingEncoding(_:)") {
  // FIXME
}

NSStringAPIs.test("stringByResolvingSymlinksInPath") {
  // FIXME
}

NSStringAPIs.test("stringByStandardizingPath") {
  // FIXME
}

NSStringAPIs.test("stringByTrimmingCharactersInSet(_:)") {
  // FIXME
}

NSStringAPIs.test("stringsByAppendingPaths(_:)") {
  // FIXME
}

NSStringAPIs.test("substringFromIndex(_:)") {
  // FIXME
}

NSStringAPIs.test("substringToIndex(_:)") {
  // FIXME
}

NSStringAPIs.test("substringWithRange(_:)") {
  // FIXME
}

NSStringAPIs.test("uppercaseStringWithLocale(_:)") {
  // FIXME
}

NSStringAPIs.test("writeToFile(_:atomically:encoding:error:)") {
  // FIXME
}

NSStringAPIs.test("writeToURL(_:atomically:encoding:error:)") {
  // FIXME
}

NSStringAPIs.test("OperatorEquals") {
  // FIXME

  // NSString == NSString
  // String == NSString
  // NSString == String
}

NSStringAPIs.run()
// CHECK: NSStringAPIs: All tests passed

var CStringTests = TestCase("CStringTests")

func getNullCString() -> CString {
  return CString(UnsafePointer.null())
}

func getASCIICString() -> (CString, dealloc: ()->()) {
  var up = UnsafePointer<UInt8>.alloc(100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (CString(up), { up.dealloc(100) })
}

func getNonASCIICString() -> (CString, dealloc: ()->()) {
  var up = UnsafePointer<UInt8>.alloc(100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (CString(up), { up.dealloc(100) })
}

func asCCharArray(a: UInt8[]) -> CChar[] {
  return a.map { $0.asSigned() }
}

CStringTests.test("init(_:)") {
  if true {
    getNullCString()
  }
  if true {
    var (s, dealloc) = getASCIICString()
    dealloc()
  }
  if true {
    var (s, dealloc) = getNonASCIICString()
    dealloc()
  }
}

CStringTests.test("convertFromLiterals") {
  var fromEmpty: CString = ""
  var fromGraphemeCluster1: CString = "z"
  var fromGraphemeCluster2: CString = "あ"
  var fromStringLiteral1: CString = "abc"
  var fromStringLiteral2: CString = "абв"
}

CStringTests.test("getLogicValue()") {
  if true {
    var s = getNullCString()
    expectFalse(s.getLogicValue())
  }
  if true {
    var (s, dealloc) = getASCIICString()
    expectTrue(s.getLogicValue())
    dealloc()
  }
  if true {
    var (s, dealloc) = getNonASCIICString()
    expectTrue(s.getLogicValue())
    dealloc()
  }
}

CStringTests.test("persist()") {
  if true {
    var s = getNullCString()
    expectEmpty(s.persist())
  }
  if true {
    var (s, dealloc) = getASCIICString()
    expectEqual(asCCharArray([ 0x61, 0x62, 0 ]), s.persist()!)
    dealloc()
  }
  if true {
    var (s, dealloc) = getNonASCIICString()
    expectEqual(asCCharArray([ 0xd0, 0xb0, 0xd0, 0xb1, 0 ]), s.persist()!)
    dealloc()
  }
}

CStringTests.test("debugDescription") {
  if true {
    var s = getNullCString()
    expectEqual("<null C string>", s.debugDescription)
  }
  if true {
    var (s, dealloc) = getASCIICString()
    expectEqual("\"ab\"", s.debugDescription)
    dealloc()
  }
  if true {
    var (s, dealloc) = getNonASCIICString()
    expectEqual("\"аб\"", s.debugDescription)
    dealloc()
  }
}

CStringTests.test("OperatorEquals") {
  var (s1, dealloc1) = getASCIICString()
  var (s2, dealloc2) = getNonASCIICString()
  expectTrue(s1 == s1)
  expectFalse(s1 == s2)
  dealloc1()
  dealloc2()
}

CStringTests.run()
// CHECK: {{^}}CStringTests: All tests passed
