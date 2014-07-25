// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift -module-cache-path %t/clang-module-cache %s -o %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/NSStringAPI_test.txt | FileCheck %s

//
// Tests for the NSString APIs as exposed by String
//

import StdlibUnittest
import Foundation

var NSStringAPIs = TestCase("NSStringAPIs")

NSStringAPIs.test("Encodings") {
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
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
    expectOptionalEqual(
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
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
    expectOptionalEqual(
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
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
    expectOptionalEqual(
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
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
    expectOptionalEqual(
        "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
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
  expectOptionalEqual("foo, a basmati bar!",
      String.stringWithCString(
          "foo, a basmati bar!", encoding: String.defaultCStringEncoding()))
}

NSStringAPIs.test("stringWithUTF8String(_:)") {
  var s = "foo あいう"
  var up = UnsafeMutablePointer<UInt8>.alloc(100)
  var i = 0
  for b in s.utf8 {
    up[i] = b
    i++
  }
  expectOptionalEqual(s, String.stringWithUTF8String(UnsafePointer(up)))
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
    let r = s.startIndex.successor()..<s.endIndex
    expectEqual(NSComparisonResult.OrderedSame,
        s.compare("bcd", range: r))
  }
  if true {
    let s = "абвг"
    let r = s.startIndex.successor()..<s.endIndex
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
    var outputArray: [String] = [ "foo", "bar" ]
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
    var outputArray: [String] = [ "foo", "bar" ]
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

  let expectedBytes: [UInt8] = [ 0xd0, 0xb0, 0xd0, 0xb1, 0xd0, 0xb2, 0 ]
  var expectedStr: [CChar] = expectedBytes.map { CChar(bitPattern: $0) }
  expectEqual(expectedStr,
      "абв".cStringUsingEncoding(NSUTF8StringEncoding)!)
}

NSStringAPIs.test("dataUsingEncoding(_:allowLossyConversion:)") {
  expectEmpty("あいう".dataUsingEncoding(NSASCIIStringEncoding, allowLossyConversion: false))

  if true {
    let data = "あいう".dataUsingEncoding(NSUTF8StringEncoding)
    let bytes = Array(
      UnsafeBufferPointer(
        start: UnsafePointer<UInt8>(data!.bytes), length: data!.length))
    let expectedBytes: [UInt8] = [
      0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86
    ]
    expectTrue(equal(expectedBytes, bytes))
  }
}

NSStringAPIs.test("decomposedStringWithCanonicalMapping") {
  expectEqual("abc", "abc".decomposedStringWithCanonicalMapping)
  expectEqual("\u{305f}\u{3099}くてん", "だくてん".decomposedStringWithCanonicalMapping)
  expectEqual("\u{ff80}\u{ff9e}ｸﾃﾝ", "ﾀﾞｸﾃﾝ".decomposedStringWithCanonicalMapping)
}

NSStringAPIs.test("decomposedStringWithCompatibilityMapping") {
  expectEqual("abc", "abc".decomposedStringWithCompatibilityMapping)
  expectEqual("\u{30bf}\u{3099}クテン", "ﾀﾞｸﾃﾝ".decomposedStringWithCompatibilityMapping)
}

NSStringAPIs.test("enumerateLines(_:)") {
  var lines: [String] = []
  "abc\n\ndefghi\njklm".enumerateLines {
    (line: String, inout stop: Bool)
  in
    lines.append(line)
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
  var tags: [String] = []
  var tokens: [String] = []
  var sentences: [String] = []
  s.enumerateLinguisticTagsInRange(startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: NSLinguisticTaggerOptions(0),
      orthography: nil) {
    (tag: String, tokenRange: Range<String.Index>, sentenceRange: Range<String.Index>, inout stop: Bool)
  in
    tags.append(tag)
    tokens.append(s[tokenRange])
    sentences.append(s[sentenceRange])
    if tags.count == 3 {
      stop = true
    }
  }
  expectEqual(
      [ NSLinguisticTagWord, NSLinguisticTagWhitespace,
        NSLinguisticTagWord ],
      tags)
  expectEqual([ "Глокая", " ", "куздра" ], tokens)
  let sentence = s[startIndex..<endIndex]
  expectEqual([ sentence, sentence, sentence ], sentences)
}

NSStringAPIs.test("enumerateSubstringsInRange(_:options:_:)") {
  let s = "え\u{304b}\u{3099}お\u{263a}\u{fe0f}😀😊"
  let startIndex = advance(s.startIndex, 1)
  let endIndex = advance(s.startIndex, 5)
  var substrings: [String] = []
  s.enumerateSubstringsInRange(startIndex..<endIndex,
      options: NSStringEnumerationOptions.ByComposedCharacterSequences) {
    (substring: String, substringRange: Range<String.Index>,
     enclosingRange: Range<String.Index>, inout stop: Bool)
  in
    substrings.append(substring)
    expectEqual(substring, s[substringRange])
    expectEqual(substring, s[enclosingRange])
  }
  expectEqual([ "\u{304b}\u{3099}", "お", "☺️", "😀" ], substrings)
}

NSStringAPIs.test("fastestEncoding") {
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
  expectTrue(contains(availableEncodings, "abc".fastestEncoding))
}

NSStringAPIs.test("fileSystemRepresentation()") {
  if true {
    let expectedStr = map("abc\0".utf8) { Int8(bitPattern: $0) }
    expectEqual(expectedStr, "abc".fileSystemRepresentation())
  }

  // On OSX file system representation is Unicode NFD.
  // This test might need to be adjusted for other systems.
  if true {
    let expectedStr = map("\u{305f}\u{3099}くてん\0".utf8) { Int8(bitPattern: $0) }
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
    var expectedStr: [UInt8] = Array("def где ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..<endIndex, remainingRange: &remainingRange)
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
    var expectedStr: [UInt8] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(4, usedLength)
    expectEqual(remainingRange.startIndex, advance(startIndex, 4))
    expectEqual(remainingRange.endIndex, endIndex)
  }
  if true {
    // 'range' is converted completely.
    let bufferLength = 100
    var expectedStr: [UInt8] = Array("def где gh жз ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: NSUTF8StringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
    expectEqual(19, usedLength)
    expectEqual(remainingRange.startIndex, endIndex)
    expectEqual(remainingRange.endIndex, endIndex)
  }
  if true {
    // Inappropriate encoding.
    let bufferLength = 100
    var expectedStr: [UInt8] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](count: bufferLength, repeatedValue: 0xff)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: NSASCIIStringEncoding,
        options: NSStringEncodingConversionOptions(0),
        range: startIndex..<endIndex, remainingRange: &remainingRange)
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
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    var result = s.getCString(&buffer, maxLength: 100,
        encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
  if true {
    let bufferLength = 17
    var expectedStr = map("abc あかさた\0".utf8) { CChar(bitPattern: $0) }
    while (expectedStr.count != bufferLength) {
      expectedStr.append(CChar(bitPattern: 0xff))
    }
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    var result = s.getCString(&buffer, maxLength: 100,
        encoding: NSUTF8StringEncoding)
    expectTrue(result)
    expectTrue(equal(expectedStr, buffer))
  }
  if true {
    let bufferLength = 100
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
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
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    var result = s.getFileSystemRepresentation(&buffer, maxLength: 100)
    expectFalse(result)
  }
  if true {
    let bufferLength = 100
    var expectedStr = map("abc \u{305f}\u{3099}くてん\0".utf8) {
      CChar(bitPattern: $0)
    }
    while (expectedStr.count != bufferLength) {
      expectedStr.append(CChar(bitPattern: 0xff))
    }
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    expectTrue(s.getFileSystemRepresentation(&buffer, maxLength: bufferLength))
    expectTrue(equal(expectedStr, buffer))
  }
}

NSStringAPIs.test("getLineStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = advance(s.startIndex, 16)..<advance(s.startIndex, 35)
  if true {
    var outStartIndex = s.startIndex
    var outLineEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getLineStart(&outStartIndex, end: &outLineEndIndex,
        contentsEnd: &outContentsEndIndex, forRange: r)
    expectEqual("штеко будланула\nбокра и кудрячит\n",
        s[outStartIndex..<outLineEndIndex])
    expectEqual("штеко будланула\nбокра и кудрячит",
        s[outStartIndex..<outContentsEndIndex])
  }
}

NSStringAPIs.test("getParagraphStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = advance(s.startIndex, 16)..<advance(s.startIndex, 35)
  if true {
    var outStartIndex = s.startIndex
    var outEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getParagraphStart(&outStartIndex, end: &outEndIndex,
        contentsEnd: &outContentsEndIndex, forRange: r)
    expectEqual("штеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n",
        s[outStartIndex..<outEndIndex])
    expectEqual("штеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.",
        s[outStartIndex..<outContentsEndIndex])
  }
}

NSStringAPIs.test("hash") {
  var s: String = "abc"
  var nsstr: NSString = "abc"
  expectEqual(nsstr.hash, s.hash)
}

NSStringAPIs.test("stringWithBytes(_:length:encoding:)") {
  var s: String = "abc あかさた"
  expectOptionalEqual(
    s, String.stringWithBytes(s.utf8, encoding: NSUTF8StringEncoding))

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
  var bytes: [UInt8] = Array(s.utf8)
  expectOptionalEqual(s, String.stringWithBytesNoCopy(&bytes,
      length: bytes.count, encoding: NSUTF8StringEncoding,
      freeWhenDone: false))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectEmpty(String.stringWithBytesNoCopy(&bytes, length: bytes.count,
      encoding: NSASCIIStringEncoding, freeWhenDone: false))
  */

  // FIXME: add a test where this function actually returns nil.
}

NSStringAPIs.test("init(utf16CodeUnits:count:)") {
  let expected = "abc абв \u{0001F60A}"
  let chars: [unichar] = Array(expected.utf16)

  expectEqual(expected, String(utf16CodeUnits: chars, count: chars.count))
}

NSStringAPIs.test("init(utf16CodeUnitsNoCopy:count:freeWhenDone:)") {
  let expected = "abc абв \u{0001F60A}"
  let chars: [unichar] = Array(expected.utf16)

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
  let args: [CVarArgType] = [ world, 42 ]
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
  let args: [CVarArgType] = [ world, 42 ]
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, arguments: args))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.systemLocale(), arguments: args))
}

NSStringAPIs.test("lastPathComponent") {
  expectEqual("bar", "/foo/bar".lastPathComponent)
  expectEqual("абв", "/foo/абв".lastPathComponent)
}

NSStringAPIs.test("utf16Count") {
  expectEqual(1, "a".utf16Count)
  expectEqual(2, "\u{0001F60A}".utf16Count)
}

NSStringAPIs.test("lengthOfBytesUsingEncoding(_:)") {
  expectEqual(1, "a".lengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  expectEqual(2, "あ".lengthOfBytesUsingEncoding(NSShiftJISStringEncoding))
}

NSStringAPIs.test("lineRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = advance(s.startIndex, 16)..<advance(s.startIndex, 35)
  if true {
    let result = s.lineRangeForRange(r)
    expectEqual("штеко будланула\nбокра и кудрячит\n", s[result])
  }
}

NSStringAPIs.test("linguisticTagsInRange(_:scheme:options:orthography:tokenRanges:)") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = advance(s.startIndex, 5)
  let endIndex = advance(s.startIndex, 17)
  var tokenRanges: [Range<String.Index>] = []
  var tags = s.linguisticTagsInRange(startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: NSLinguisticTaggerOptions(0),
      orthography: nil, tokenRanges: &tokenRanges)
  expectEqual(
      [ NSLinguisticTagWord, NSLinguisticTagWhitespace,
        NSLinguisticTagWord ],
      tags)
  expectEqual([ "Глокая", " ", "куздра" ],
      tokenRanges.map { s[$0] } )
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
      NSLocale(localeIdentifier: "en")))
  expectEqual("абвг", "абВГ".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))

  expectEqual("たちつてと", "たちつてと".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0130}".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectEqual("\u{0069}", "\u{0130}".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "tr")))

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectEqual("\u{0069}", "\u{0049}\u{0307}".lowercaseStringWithLocale(
      NSLocale(localeIdentifier: "tr")))
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
    let s = "\u{1F60A}"
    expectLE(countElements(s.utf8),
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
}

NSStringAPIs.test("paragraphRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = advance(s.startIndex, 16)..<advance(s.startIndex, 35)
  if true {
    let result = s.paragraphRangeForRange(r)
    expectEqual("штеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n", s[result])
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
      "\u{305f}\u{3099}くてん".precomposedStringWithCanonicalMapping)
  expectEqual("ﾀﾞｸﾃﾝ",
      "\u{ff80}\u{ff9e}ｸﾃﾝ".precomposedStringWithCanonicalMapping)
  expectEqual("\u{fb03}", "\u{fb03}".precomposedStringWithCanonicalMapping)
}

NSStringAPIs.test("precomposedStringWithCompatibilityMapping") {
  expectEqual("abc", "abc".precomposedStringWithCompatibilityMapping)
  /*
  Test disabled because of:
  <rdar://problem/17041347> NFKD normalization as implemented by
  'precomposedStringWithCompatibilityMapping:' is not idempotent

  expectEqual("\u{30c0}クテン",
      "\u{ff80}\u{ff9e}ｸﾃﾝ".precomposedStringWithCompatibilityMapping)
  */
  expectEqual("ffi", "\u{fb03}".precomposedStringWithCompatibilityMapping)
}

NSStringAPIs.test("propertyList()") {
  expectEqual([ "foo", "bar" ],
      "(\"foo\", \"bar\")".propertyList() as [String])
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
    if true {
      let s = "абвклмнабвклмн"
      let r = s.rangeOfCharacterFromSet(charset,
          options: .BackwardsSearch)!
      expectEqual(advance(s.startIndex, 9), r.startIndex)
      expectEqual(advance(s.startIndex, 10), r.endIndex)
    }
    if true {
      let s = "абвклмнабв"
      let r = s.rangeOfCharacterFromSet(charset,
          range: advance(s.startIndex, 3)..<s.endIndex)!
      expectEqual(advance(s.startIndex, 7), r.startIndex)
      expectEqual(advance(s.startIndex, 8), r.endIndex)
    }
  }

  if true {
    let charset = NSCharacterSet(charactersInString: "\u{305f}\u{3099}")
    expectEmpty("\u{3060}".rangeOfCharacterFromSet(charset))
  }
  if true {
    let charset = NSCharacterSet(charactersInString: "\u{3060}")
    expectEmpty("\u{305f}\u{3099}".rangeOfCharacterFromSet(charset))
  }

  if true {
    let charset = NSCharacterSet(charactersInString: "\u{1F600}")
    if true {
      let s = "abc\u{1F600}"
      expectEqual("\u{1F600}",
          s[s.rangeOfCharacterFromSet(charset)!])
    }
    if true {
      expectEmpty("abc\u{1F601}".rangeOfCharacterFromSet(charset))
    }
  }
}

NSStringAPIs.test("rangeOfComposedCharacterSequenceAtIndex(_:)") {
  let s = "\u{1F601}abc \u{305f}\u{3099} def"
  expectEqual("\u{1F601}", s[s.rangeOfComposedCharacterSequenceAtIndex(
      s.startIndex)])
  expectEqual("a", s[s.rangeOfComposedCharacterSequenceAtIndex(
      advance(s.startIndex, 1))])
  expectEqual("\u{305f}\u{3099}", s[s.rangeOfComposedCharacterSequenceAtIndex(
      advance(s.startIndex, 5))])
  expectEqual(" ", s[s.rangeOfComposedCharacterSequenceAtIndex(
      advance(s.startIndex, 6))])
}

NSStringAPIs.test("rangeOfComposedCharacterSequencesForRange(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("\u{1F601}a", s[s.rangeOfComposedCharacterSequencesForRange(
      s.startIndex..<advance(s.startIndex, 2))])
  expectEqual("せ\u{3099}そ\u{3099}", s[s.rangeOfComposedCharacterSequencesForRange(
      advance(s.startIndex, 8)..<advance(s.startIndex, 10))])
}

NSStringAPIs.test("rangeOfString(_:options:range:locale:)") {
  if true {
    let s = ""
    expectEmpty(s.rangeOfString(""))
    expectEmpty(s.rangeOfString("abc"))
  }
  if true {
    let s = "abc"
    expectEmpty(s.rangeOfString(""))
    expectEmpty(s.rangeOfString("def"))
    expectOptionalEqual(s.startIndex..<s.endIndex, s.rangeOfString("abc"))
  }
  if true {
    let s = "さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"
    expectEqual("す\u{3099}", s[s.rangeOfString("す\u{3099}")!])
    expectEqual("す\u{3099}", s[s.rangeOfString("\u{305a}")!])

    expectEmpty(s.rangeOfString("\u{3099}す"))
    expectEmpty(s.rangeOfString("す"))

    // Note: here `rangeOfString` API produces indexes that don't point between
    // grapheme cluster boundaries -- these can not be created with public
    // String interface.
    //
    // FIXME: why does this seach succeed and the above queries fail?  There is
    // no apparent pattern.
    expectEqual("\u{3099}", s[s.rangeOfString("\u{3099}")!])
  }
  if true {
    let s = "а\u{0301}б\u{0301}в\u{0301}г\u{0301}"
    expectEqual("а\u{0301}", s[s.rangeOfString("а\u{0301}")!])
    expectEqual("б\u{0301}", s[s.rangeOfString("б\u{0301}")!])

    expectEmpty(s.rangeOfString("б"))
    expectEmpty(s.rangeOfString("\u{0301}б"))

    // Again, indexes that don't correspond to grapheme cluster boundaries.
    expectEqual("\u{0301}", s[s.rangeOfString("\u{0301}")!])
  }
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
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.substringFromIndex(s.startIndex))
  expectEqual("せ\u{3099}そ\u{3099}",
      s.substringFromIndex(advance(s.startIndex, 8)))
  expectEqual("", s.substringFromIndex(advance(s.startIndex, 10)))
}

NSStringAPIs.test("substringToIndex(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substringToIndex(s.startIndex))
  expectEqual("\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}",
      s.substringToIndex(advance(s.startIndex, 8)))
  expectEqual(s, s.substringToIndex(advance(s.startIndex, 10)))
}

NSStringAPIs.test("substringWithRange(_:)") {
  // FIXME
}

NSStringAPIs.test("uppercaseStringWithLocale(_:)") {
  expectEqual("ABCD", "abCD".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  expectEqual("АБВГ", "абВГ".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))
  expectEqual("АБВГ", "абВГ".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))

  expectEqual("たちつてと", "たちつてと".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectEqual("\u{0049}", "\u{0069}".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectEqual("\u{0130}", "\u{0069}".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "tr")))

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectEqual("\u{0053}\u{0053}", "\u{00df}".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "en")))

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectEqual("\u{0046}\u{0049}", "\u{fb01}".uppercaseStringWithLocale(
      NSLocale(localeIdentifier: "ru")))
}

NSStringAPIs.test("writeToFile(_:atomically:encoding:error:)") {
  // FIXME
}

NSStringAPIs.test("writeToURL(_:atomically:encoding:error:)") {
  // FIXME
}

func checkEqualityImpl(
  expectedEqualNFD: Bool, lhs: String, rhs: String,
  stackTrace: SourceLocStack
) {
  // String / String
  expectEqual(expectedEqualNFD, lhs == rhs, stackTrace: stackTrace)
  expectEqual(!expectedEqualNFD, lhs != rhs, stackTrace: stackTrace)
  checkHashable(expectedEqualNFD, lhs, rhs, stackTrace.withCurrentLoc())

  // NSString / NSString
  let lhsNSString = lhs as NSString
  let rhsNSString = rhs as NSString
  let expectedEqualUnicodeScalars =
    Array(lhs.unicodeScalars) == Array(rhs.unicodeScalars)
  expectEqual(
    expectedEqualUnicodeScalars, lhsNSString == rhsNSString,
    stackTrace: stackTrace)
  expectEqual(
    !expectedEqualUnicodeScalars, lhsNSString != rhsNSString,
    stackTrace: stackTrace)
  checkHashable(
    expectedEqualUnicodeScalars, lhsNSString, rhsNSString,
    stackTrace.withCurrentLoc())

  // String / NSString
  expectEqual(expectedEqualNFD, lhs == rhsNSString, stackTrace: stackTrace)
  expectEqual(!expectedEqualNFD, lhs != rhsNSString, stackTrace: stackTrace)

  // NSString / String
  expectEqual(expectedEqualNFD, lhs == rhsNSString, stackTrace: stackTrace)
  expectEqual(!expectedEqualNFD, lhs != rhsNSString, stackTrace: stackTrace)
}

func checkEquality(
  expectedEqualNFD: Bool, lhs: String, rhs: String, stackTrace: SourceLocStack
) {
  checkEqualityImpl(expectedEqualNFD, lhs, rhs, stackTrace.withCurrentLoc())
  checkEqualityImpl(expectedEqualNFD, rhs, lhs, stackTrace.withCurrentLoc())
}

struct EqualityTest {
  let expectedEqualNFD: Bool
  let lhs: String
  let rhs: String
  let loc: SourceLoc

  init(_ expectedEqualNFD: Bool, _ lhs: String, _ rhs: String,
       file: String = __FILE__, line: UWord = __LINE__) {
    self.expectedEqualNFD = expectedEqualNFD
    self.lhs = lhs
    self.rhs = rhs
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

let equalityTests = [
  EqualityTest(true, "", ""),
  EqualityTest(false, "a", ""),

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  EqualityTest(true, "a\u{301}", "\u{e1}"),
  EqualityTest(false, "a\u{301}", "a"),
  EqualityTest(false, "\u{e1}", "a"),

  // U+304B HIRAGANA LETTER KA
  // U+304C HIRAGANA LETTER GA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  EqualityTest(true, "\u{304b}", "\u{304b}"),
  EqualityTest(true, "\u{304c}", "\u{304c}"),
  EqualityTest(false, "\u{304b}", "\u{304c}"),
  EqualityTest(false, "\u{304b}", "\u{304c}\u{3099}"),
  EqualityTest(true, "\u{304c}", "\u{304b}\u{3099}"),
  EqualityTest(false, "\u{304c}", "\u{304c}\u{3099}"),

  // U+212B ANGSTROM SIGN
  // U+030A COMBINING RING ABOVE
  // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  EqualityTest(true, "\u{212b}", "A\u{30a}"),
  EqualityTest(true, "\u{212b}", "\u{c5}"),
  EqualityTest(true, "A\u{30a}", "\u{c5}"),
  EqualityTest(false, "A\u{30a}", "a"),

  // U+2126 OHM SIGN
  // U+03A9 GREEK CAPITAL LETTER OMEGA
  EqualityTest(true, "\u{2126}", "\u{03a9}"),

  // U+1E69 LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  EqualityTest(true, "\u{1e69}", "s\u{323}\u{307}"),
  EqualityTest(true, "\u{1e69}", "s\u{307}\u{323}"),
  EqualityTest(true, "\u{1e69}", "\u{1e63}\u{307}"),
  EqualityTest(true, "\u{1e63}\u{307}", "s\u{323}\u{307}"),
  EqualityTest(true, "\u{1e63}\u{307}", "s\u{307}\u{323}"),

  // U+FB01 LATIN SMALL LIGATURE FI
  EqualityTest(true, "\u{fb01}", "\u{fb01}"),
  EqualityTest(false, "\u{fb01}", "fi"),

  // Test that Unicode collation is performed in deterministic mode.
  //
  // U+0301 COMBINING ACUTE ACCENT
  // U+0341 COMBINING ACUTE TONE MARK
  // U+0954 DEVANAGARI ACUTE ACCENT
  //
  // Collation elements from DUCET:
  // 0301  ; [.0000.0024.0002] # COMBINING ACUTE ACCENT
  // 0341  ; [.0000.0024.0002] # COMBINING ACUTE TONE MARK
  // 0954  ; [.0000.0024.0002] # DEVANAGARI ACUTE ACCENT
  //
  // U+0301 and U+0954 don't decompose in the canonical decomposition mapping.
  // U+0341 has a canonical decomposition mapping of U+0301.
  EqualityTest(true, "\u{0301}", "\u{0341}"),
  EqualityTest(false, "\u{0301}", "\u{0954}"),
  EqualityTest(false, "\u{0341}", "\u{0954}"),
]

NSStringAPIs.test("OperatorEquals") {
  for test in equalityTests {
    checkEquality(
      test.expectedEqualNFD, test.lhs, test.rhs, test.loc.withCurrentLoc())
  }
}

// FIXME: these properties should be implemented in the core library.
// <rdar://problem/17550602> [unicode] Implement case folding
NSStringAPIs.test("lowercaseString") {
  expectEqual("abcd", "abCD".lowercaseString)
  expectEqual("абвг", "абВГ".lowercaseString)
  expectEqual("たちつてと", "たちつてと".lowercaseString)

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0130}".lowercaseString)

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercaseString)
}

NSStringAPIs.test("uppercaseString") {
  expectEqual("ABCD", "abCD".uppercaseString)
  expectEqual("АБВГ", "абВГ".uppercaseString)
  expectEqual("たちつてと", "たちつてと".uppercaseString)

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectEqual("\u{0049}", "\u{0069}".uppercaseString)

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectEqual("\u{0053}\u{0053}", "\u{00df}".uppercaseString)

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectEqual("\u{0046}\u{0049}", "\u{fb01}".uppercaseString)
}

NSStringAPIs.run()
// CHECK: NSStringAPIs: All tests passed

var CStringTests = TestCase("CStringTests")

func getNullCString() -> UnsafeMutablePointer<CChar> {
  return .null()
}

func getASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: ()->()) {
  var up = UnsafeMutablePointer<CChar>.alloc(100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (up, { up.dealloc(100) })
}

func getNonASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: ()->()) {
  var up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String1() -> (UnsafeMutablePointer<CChar>, dealloc: ()->()) {
  var up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x80
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String2() -> (UnsafeMutablePointer<CChar>, dealloc: ()->()) {
  var up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x81
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func asCCharArray(a: [UInt8]) -> [CChar] {
  return a.map { CChar(bitPattern: $0) }
}

CStringTests.test("String.fromCString") {
  if true {
    let s = getNullCString()
    expectEmpty(String.fromCString(s))
  }
  if true {
    let (s, dealloc) = getASCIICString()
    expectOptionalEqual("ab", String.fromCString(s))
    dealloc()
  }
  if true {
    let (s, dealloc) = getNonASCIICString()
    expectOptionalEqual("аб", String.fromCString(s))
    dealloc()
  }
  if true {
    let (s, dealloc) = getIllFormedUTF8String1()
    expectEmpty(String.fromCString(s))
    dealloc()
  }
}

CStringTests.test("String.fromCStringRepairingIllFormedUTF8") {
  if true {
    let s = getNullCString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectEmpty(result)
    expectFalse(hadError)
  }
  if true {
    let (s, dealloc) = getASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("ab", result)
    expectFalse(hadError)
    dealloc()
  }
  if true {
    let (s, dealloc) = getNonASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("аб", result)
    expectFalse(hadError)
    dealloc()
  }
  if true {
    let (s, dealloc) = getIllFormedUTF8String1()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
    expectTrue(hadError)
    dealloc()
  }
}

CStringTests.run()
// CHECK: {{^}}CStringTests: All tests passed
