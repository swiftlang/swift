// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

//
// Tests for the NSString APIs as exposed by String
//

import StdlibUnittest


import Foundation
import StdlibUnittestFoundationExtras

// The most simple subclass of NSString that CoreFoundation does not know
// about.
class NonContiguousNSString : NSString {
  required init(coder aDecoder: NSCoder) {
    fatalError("don't call this initializer")
  }

  override init() { 
    _value = []
    super.init() 
  }

  init(_ value: [UInt16]) {
    _value = value
    super.init()
  }

  @objc(copyWithZone:) override func copy(with zone: NSZone?) -> Any {
    // Ensure that copying this string produces a class that CoreFoundation
    // does not know about.
    return self
  }

  @objc override var length: Int {
    return _value.count
  }

  @objc override func character(at index: Int) -> unichar {
    return _value[index]
  }

  var _value: [UInt16]
}

let temporaryFileContents =
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit,\n" +
  "sed do eiusmod tempor incididunt ut labore et dolore magna\n" +
  "aliqua.\n"

func createNSStringTemporaryFile()
  -> (existingPath: String, nonExistentPath: String) {
  let existingPath =
    createTemporaryFile("NSStringAPIs.", ".txt", temporaryFileContents)
  let nonExistentPath = existingPath + "-NoNeXiStEnT"
  return (existingPath, nonExistentPath)
}

var NSStringAPIs = TestSuite("NSStringAPIs")

NSStringAPIs.test("Encodings") {
  let availableEncodings: [String.Encoding] = String.availableStringEncodings
  expectNotEqual(0, availableEncodings.count)

  let defaultCStringEncoding = String.defaultCStringEncoding
  expectTrue(availableEncodings.contains(defaultCStringEncoding))

  expectNotEqual("", String.localizedName(of: .utf8))
}

NSStringAPIs.test("NSStringEncoding") {
  // Make sure NSStringEncoding and its values are type-compatible.
  var enc: String.Encoding
  enc = .windowsCP1250
  enc = .utf32LittleEndian
  enc = .utf32BigEndian
  enc = .ascii
  enc = .utf8
}

NSStringAPIs.test("localizedStringWithFormat(_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String.localizedStringWithFormat(
    "Hello, %@!%%%ld", world, 42))

  withOverriddenLocaleCurrentLocale("en_US") {
    expectEqual("0.5", String.localizedStringWithFormat("%g", 0.5))
  }

  withOverriddenLocaleCurrentLocale("uk") {
    expectEqual("0,5", String.localizedStringWithFormat("%g", 0.5))
  }
}

NSStringAPIs.test("init(contentsOfFile:encoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()

  do {
    let content = try String(
      contentsOfFile: existingPath, encoding: .ascii)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  do {
    let content = try String(
      contentsOfFile: nonExistentPath, encoding: .ascii)
    expectUnreachable()
  } catch {
  }
}

NSStringAPIs.test("init(contentsOfFile:usedEncoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()

  do {
    var usedEncoding: String.Encoding = String.Encoding(rawValue: 0)
    let content = try String(
      contentsOfFile: existingPath, usedEncoding: &usedEncoding)
    expectNotEqual(0, usedEncoding.rawValue)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  var usedEncoding: String.Encoding = String.Encoding(rawValue: 0)
  do {
    _ = try String(contentsOfFile: nonExistentPath)
    expectUnreachable()
  } catch {
    expectEqual(0, usedEncoding.rawValue)
  }
}


NSStringAPIs.test("init(contentsOf:encoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = URL(string: "file://" + existingPath)!
  let nonExistentURL = URL(string: "file://" + nonExistentPath)!
  do {
    let content = try String(
      contentsOf: existingURL, encoding: .ascii)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  do {
    _ = try String(contentsOf: nonExistentURL, encoding: .ascii)
    expectUnreachable()
  } catch {
  }
}

NSStringAPIs.test("init(contentsOf:usedEncoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = URL(string: "file://" + existingPath)!
  let nonExistentURL = URL(string: "file://" + nonExistentPath)!
  do {
    var usedEncoding: String.Encoding = String.Encoding(rawValue: 0)
    let content = try String(
      contentsOf: existingURL, usedEncoding: &usedEncoding)

    expectNotEqual(0, usedEncoding.rawValue)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  var usedEncoding: String.Encoding = String.Encoding(rawValue: 0)
  do {
    _ = try String(contentsOf: nonExistentURL, usedEncoding: &usedEncoding)
    expectUnreachable()
  } catch {
    expectEqual(0, usedEncoding.rawValue)
  }
}

NSStringAPIs.test("init(cString_:encoding:)") {
  expectOptionalEqual("foo, a basmati bar!",
      String(cString: 
          "foo, a basmati bar!", encoding: String.defaultCStringEncoding))
}

NSStringAPIs.test("init(utf8String:)") {
  var s = "foo あいう"
  var up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  var i = 0
  for b in s.utf8 {
    up[i] = b
    i += 1
  }
  up[i] = 0
  let cstr = UnsafeMutableRawPointer(up)
    .bindMemory(to: CChar.self, capacity: 100)
  expectOptionalEqual(s, String(utf8String: cstr))
  up.deallocate(capacity: 100)
}

NSStringAPIs.test("canBeConvertedToEncoding(_:)") {
  expectTrue("foo".canBeConverted(to: .ascii))
  expectFalse("あいう".canBeConverted(to: .ascii))
}

NSStringAPIs.test("capitalized") {
  expectEqual("Foo Foo Foo Foo", "foo Foo fOO FOO".capitalized)
  expectEqual("Жжж", "жжж".capitalized)
}

NSStringAPIs.test("localizedCapitalized") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenLocaleCurrentLocale("en") { () -> Void in
      expectEqual(
        "Foo Foo Foo Foo",
        "foo Foo fOO FOO".localizedCapitalized)
      expectEqual("Жжж", "жжж".localizedCapitalized)
      return ()
    }

    //
    // Special casing.
    //

    // U+0069 LATIN SMALL LETTER I
    // to upper case:
    // U+0049 LATIN CAPITAL LETTER I
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("Iii Iii", "iii III".localizedCapitalized)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenLocaleCurrentLocale("tr") {
      expectEqual("\u{0130}ii Iıı", "iii III".localizedCapitalized)
    }
  }
}

/// Checks that executing the operation in the locale with the given
/// `localeID` (or if `localeID` is `nil`, the current locale) gives
/// the expected result, and that executing the operation with a nil
/// locale gives the same result as explicitly passing the system
/// locale.
///
/// - Parameter expected: the expected result when the operation is
///   executed in the given localeID
func expectLocalizedEquality(
  _ expected: String,
  _ op: (_: Locale?) -> String,
  _ localeID: String? = nil,
  _ message: @autoclosure () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),
  file: String = #file, line: UInt = #line
) {
  let trace = stackTrace.pushIf(showFrame, file: file, line: line)

  let locale = localeID.map {
    Locale(identifier: $0)
  } ?? Locale.current
  
  expectEqual(
    expected, op(locale),
    message(), stackTrace: trace)  
}

NSStringAPIs.test("capitalizedString(with:)") {
  expectLocalizedEquality(
    "Foo Foo Foo Foo",
    { loc in "foo Foo fOO FOO".capitalized(with: loc) })
  
  expectLocalizedEquality("Жжж", { loc in "жжж".capitalized(with: loc) })

  expectEqual(
    "Foo Foo Foo Foo",
    "foo Foo fOO FOO".capitalized(with: nil))
  expectEqual("Жжж", "жжж".capitalized(with: nil))

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality(
    "Iii Iii",
    { loc in "iii III".capitalized(with: loc) }, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality(
    "İii Iıı",
    { loc in "iii III".capitalized(with: loc) }, "tr")
}

NSStringAPIs.test("caseInsensitiveCompare(_:)") {
  expectEqual(ComparisonResult.orderedSame,
      "abCD".caseInsensitiveCompare("AbCd"))
  expectEqual(ComparisonResult.orderedAscending,
      "abCD".caseInsensitiveCompare("AbCdE"))

  expectEqual(ComparisonResult.orderedSame,
      "абвг".caseInsensitiveCompare("АбВг"))
  expectEqual(ComparisonResult.orderedAscending,
      "абВГ".caseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("commonPrefix(with:options:)") {
  expectEqual("ab",
      "abcd".commonPrefix(with: "abdc", options: []))
  expectEqual("abC",
      "abCd".commonPrefix(with: "abce", options: .caseInsensitive))

  expectEqual("аб",
      "абвг".commonPrefix(with: "абгв", options: []))
  expectEqual("абВ",
      "абВг".commonPrefix(with: "абвд", options: .caseInsensitive))
}

NSStringAPIs.test("compare(_:options:range:locale:)") {
  expectEqual(ComparisonResult.orderedSame,
      "abc".compare("abc"))
  expectEqual(ComparisonResult.orderedAscending,
      "абв".compare("где"))

  expectEqual(ComparisonResult.orderedSame,
      "abc".compare("abC", options: .caseInsensitive))
  expectEqual(ComparisonResult.orderedSame,
      "абв".compare("абВ", options: .caseInsensitive))

  do {
    let s = "abcd"
    let r = s.index(after: s.startIndex)..<s.endIndex
    expectEqual(ComparisonResult.orderedSame,
        s.compare("bcd", range: r))
  }
  do {
    let s = "абвг"
    let r = s.index(after: s.startIndex)..<s.endIndex
    expectEqual(ComparisonResult.orderedSame,
        s.compare("бвг", range: r))
  }

  expectEqual(ComparisonResult.orderedSame,
      "abc".compare("abc", locale: Locale.current))
  expectEqual(ComparisonResult.orderedSame,
      "абв".compare("абв", locale: Locale.current))
}

NSStringAPIs.test("completePath(into:caseSensitive:matchesInto:filterTypes)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  do {
    var count = nonExistentPath.completePath(caseSensitive: false)
    expectEqual(0, count)
  }

  do {
    var outputName = "None Found"
    var count = nonExistentPath.completePath(
        into: &outputName, caseSensitive: false)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = ["foo", "bar"]
    var count = nonExistentPath.completePath(
        into: &outputName, caseSensitive: false, matchesInto: &outputArray)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
    expectEqual(["foo", "bar"], outputArray)
  }

  do {
    var count = existingPath.completePath(caseSensitive: false)
    expectEqual(1, count)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePath(
        into: &outputName, caseSensitive: false)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = ["foo", "bar"]
    var count = existingPath.completePath(
        into: &outputName, caseSensitive: false, matchesInto: &outputArray)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
    expectEqual([existingPath], outputArray)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePath(
        into: &outputName, caseSensitive: false, filterTypes: ["txt"])

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }
}

NSStringAPIs.test("components(separatedBy:) (NSCharacterSet)") {
  expectEqual([""], "".components(
    separatedBy: CharacterSet.decimalDigits))

  expectEqual(
    ["абв", "", "あいう", "abc"],
    "абв12あいう3abc".components(
        separatedBy: CharacterSet.decimalDigits))

  expectEqual(
    ["абв", "", "あいう", "abc"],
    "абв\u{1F601}\u{1F602}あいう\u{1F603}abc"
      .components(
        separatedBy: CharacterSet(charactersIn: "\u{1F601}\u{1F602}\u{1F603}")))

  // Performs Unicode scalar comparison.
  expectEqual(
    ["abcし\u{3099}def"],
    "abcし\u{3099}def".components(
      separatedBy: CharacterSet(charactersIn: "\u{3058}")))
}

NSStringAPIs.test("components(separatedBy:) (String)") {
  expectEqual([""], "".components(separatedBy: "//"))

  expectEqual(
    ["абв", "あいう", "abc"],
    "абв//あいう//abc".components(separatedBy: "//"))

  // Performs normalization.
  expectEqual(
    ["abc", "def"],
    "abcし\u{3099}def".components(separatedBy: "\u{3058}"))
}

NSStringAPIs.test("cString(usingEncoding:)") {
  expectNil("абв".cString(using: .ascii))

  let expectedBytes: [UInt8] = [ 0xd0, 0xb0, 0xd0, 0xb1, 0xd0, 0xb2, 0 ]
  var expectedStr: [CChar] = expectedBytes.map { CChar(bitPattern: $0) }
  expectEqual(expectedStr,
      "абв".cString(using: .utf8)!)
}

NSStringAPIs.test("data(usingEncoding:allowLossyConversion:)") {
  expectNil("あいう".data(using: .ascii, allowLossyConversion: false))

  do {
    let data = "あいう".data(using: .utf8)!
    let expectedBytes: [UInt8] = [
        0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86
    ]
    expectEqualSequence(expectedBytes, data)
  }
}

NSStringAPIs.test("init(data:encoding:)") {
  let bytes: [UInt8] = [0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86]
  let data = Data(bytes: bytes)
  
  expectNil(String(data: data, encoding: .nonLossyASCII))
  
  expectEqualSequence(
    "あいう".characters, 
    String(data: data, encoding: .utf8)!.characters)
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
    (line: String, stop: inout Bool)
  in
    lines.append(line)
    if lines.count == 3 {
      stop = true
    }
  }
  expectEqual(["abc", "", "defghi"], lines)
}

NSStringAPIs.test("enumerateLinguisticTagsIn(_:scheme:options:orthography:_:") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = s.index(s.startIndex, offsetBy: 5)
  let endIndex = s.index(s.startIndex, offsetBy: 62)
  var tags: [String] = []
  var tokens: [String] = []
  var sentences: [String] = []
  s.enumerateLinguisticTags(in: startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: [],
      orthography: nil) {
    (tag: String, tokenRange: Range<String.Index>, sentenceRange: Range<String.Index>, stop: inout Bool)
  in
    tags.append(tag)
    tokens.append(s[tokenRange])
    sentences.append(s[sentenceRange])
    if tags.count == 3 {
      stop = true
    }
  }
  expectEqual(
    [NSLinguisticTagWord, NSLinguisticTagWhitespace, NSLinguisticTagWord],
    tags)
  expectEqual(["Глокая", " ", "куздра"], tokens)
  let sentence = s[startIndex..<endIndex]
  expectEqual([sentence, sentence, sentence], sentences)
}

NSStringAPIs.test("enumerateSubstringsIn(_:options:_:)") {
  let s = "え\u{304b}\u{3099}お\u{263a}\u{fe0f}😀😊"
  let startIndex = s.index(s.startIndex, offsetBy: 1)
  let endIndex = s.index(s.startIndex, offsetBy: 5)
  do {
    var substrings: [String] = []
    s.enumerateSubstrings(in: startIndex..<endIndex,
      options: String.EnumerationOptions.byComposedCharacterSequences) {
      (substring: String?, substringRange: Range<String.Index>,
       enclosingRange: Range<String.Index>, stop: inout Bool)
    in
      substrings.append(substring!)
      expectEqual(substring, s[substringRange])
      expectEqual(substring, s[enclosingRange])
    }
    expectEqual(["\u{304b}\u{3099}", "お", "☺️", "😀"], substrings)
  }
  do {
    var substrings: [String] = []
    s.enumerateSubstrings(in: startIndex..<endIndex,
      options: [.byComposedCharacterSequences, .substringNotRequired]) {
      (substring_: String?, substringRange: Range<String.Index>,
       enclosingRange: Range<String.Index>, stop: inout Bool)
    in
      expectNil(substring_)
      let substring = s[substringRange]
      substrings.append(substring)
      expectEqual(substring, s[enclosingRange])
    }
    expectEqual(["\u{304b}\u{3099}", "お", "☺️", "😀"], substrings)
  }
}

NSStringAPIs.test("fastestEncoding") {
  let availableEncodings: [String.Encoding] = String.availableStringEncodings
  expectTrue(availableEncodings.contains("abc".fastestEncoding))
}

NSStringAPIs.test("getBytes(_:maxLength:usedLength:encoding:options:range:remaining:)") {
  let s = "abc абв def где gh жз zzz"
  let startIndex = s.index(s.startIndex, offsetBy: 8)
  let endIndex = s.index(s.startIndex, offsetBy: 22)
  do {
    // 'maxLength' is limiting.
    let bufferLength = 100
    var expectedStr: [UInt8] = Array("def где ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: .utf8,
        options: [],
        range: startIndex..<endIndex, remaining: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(11, usedLength)
    expectEqual(remainingRange.lowerBound, s.index(startIndex, offsetBy: 8))
    expectEqual(remainingRange.upperBound, endIndex)
  }
  do {
    // 'bufferLength' is limiting.  Note that the buffer is not filled
    // completely, since doing that would break a UTF sequence.
    let bufferLength = 5
    var expectedStr: [UInt8] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: 11, usedLength: &usedLength,
        encoding: .utf8,
        options: [],
        range: startIndex..<endIndex, remaining: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(4, usedLength)
    expectEqual(remainingRange.lowerBound, s.index(startIndex, offsetBy: 4))
    expectEqual(remainingRange.upperBound, endIndex)
  }
  do {
    // 'range' is converted completely.
    let bufferLength = 100
    var expectedStr: [UInt8] = Array("def где gh жз ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: .utf8,
        options: [],
        range: startIndex..<endIndex, remaining: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(19, usedLength)
    expectEqual(remainingRange.lowerBound, endIndex)
    expectEqual(remainingRange.upperBound, endIndex)
  }
  do {
    // Inappropriate encoding.
    let bufferLength = 100
    var expectedStr: [UInt8] = Array("def ".utf8)
    while (expectedStr.count != bufferLength) {
      expectedStr.append(0xff)
    }
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
    var usedLength = 0
    var remainingRange = startIndex..<endIndex
    var result = s.getBytes(&buffer, maxLength: bufferLength,
        usedLength: &usedLength, encoding: .ascii,
        options: [],
        range: startIndex..<endIndex, remaining: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(4, usedLength)
    expectEqual(remainingRange.lowerBound, s.index(startIndex, offsetBy: 4))
    expectEqual(remainingRange.upperBound, endIndex)
  }
}

NSStringAPIs.test("getCString(_:maxLength:encoding:)") {
  var s = "abc あかさた"
  do {
    // The largest buffer that cannot accommodate the string plus null terminator.
    let bufferLength = 16
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = s.getCString(&buffer, maxLength: 100,
      encoding: .utf8)
    expectFalse(result)
  }
  do {
    // The smallest buffer where the result can fit.
    let bufferLength = 17
    var expectedStr = "abc あかさた\0".utf8.map { CChar(bitPattern: $0) }
    while (expectedStr.count != bufferLength) {
      expectedStr.append(CChar(bitPattern: 0xff))
    }
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = s.getCString(&buffer, maxLength: 100,
      encoding: .utf8)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
  }
  do {
    // Limit buffer size with 'maxLength'.
    let bufferLength = 100
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = s.getCString(&buffer, maxLength: 8,
      encoding: .utf8)
    expectFalse(result)
  }
  do {
    // String with unpaired surrogates.
    let illFormedUTF16 = NonContiguousNSString([ 0xd800 ]) as String
    let bufferLength = 100
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = illFormedUTF16.getCString(&buffer, maxLength: 100,
      encoding: .utf8)
    expectFalse(result)
  }
}

NSStringAPIs.test("getLineStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = s.index(s.startIndex, offsetBy: 16)..<s.index(s.startIndex, offsetBy: 35)
  do {
    var outStartIndex = s.startIndex
    var outLineEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getLineStart(&outStartIndex, end: &outLineEndIndex,
        contentsEnd: &outContentsEndIndex, for: r)
    expectEqual("штеко будланула\nбокра и кудрячит\n",
        s[outStartIndex..<outLineEndIndex])
    expectEqual("штеко будланула\nбокра и кудрячит",
        s[outStartIndex..<outContentsEndIndex])
  }
}

NSStringAPIs.test("getParagraphStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = s.index(s.startIndex, offsetBy: 16)..<s.index(s.startIndex, offsetBy: 35)
  do {
    var outStartIndex = s.startIndex
    var outEndIndex = s.startIndex
    var outContentsEndIndex = s.startIndex
    s.getParagraphStart(&outStartIndex, end: &outEndIndex,
        contentsEnd: &outContentsEndIndex, for: r)
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

NSStringAPIs.test("init(bytes:encoding:)") {
  var s: String = "abc あかさた"
  expectOptionalEqual(
    s, String(bytes: s.utf8, encoding: .utf8))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectNil(String(bytes: bytes, length: bytes.count,
      encoding: .ascii))
  */

  // FIXME: add a test where this function actually returns nil.
}

NSStringAPIs.test("init(bytesNoCopy:length:encoding:freeWhenDone:)") {
  var s: String = "abc あかさた"
  var bytes: [UInt8] = Array(s.utf8)
  expectOptionalEqual(s, String(bytesNoCopy: &bytes,
      length: bytes.count, encoding: .utf8,
      freeWhenDone: false))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectNil(String(bytesNoCopy: &bytes, length: bytes.count,
      encoding: .ascii, freeWhenDone: false))
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
  expectEqual("", String(format: ""))
  expectEqual(
    "abc абв \u{0001F60A}", String(format: "abc абв \u{0001F60A}"))

  let world: NSString = "world"
  expectEqual("Hello, world!%42",
      String(format: "Hello, %@!%%%ld", world, 42))

  // test for rdar://problem/18317906
  expectEqual("3.12", String(format: "%.2f", 3.123456789))
  expectEqual("3.12", NSString(format: "%.2f", 3.123456789))
}

NSStringAPIs.test("init(format:arguments:)") {
  expectEqual("", String(format: "", arguments: []))
  expectEqual(
    "abc абв \u{0001F60A}",
    String(format: "abc абв \u{0001F60A}", arguments: []))

  let world: NSString = "world"
  let args: [CVarArg] = [ world, 42 ]
  expectEqual("Hello, world!%42",
      String(format: "Hello, %@!%%%ld", arguments: args))
}

NSStringAPIs.test("init(format:locale:_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, world, 42))
}

NSStringAPIs.test("init(format:locale:arguments:)") {
  let world: NSString = "world"
  let args: [CVarArg] = [ world, 42 ]
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, arguments: args))
}

NSStringAPIs.test("utf16Count") {
  expectEqual(1, "a".utf16.count)
  expectEqual(2, "\u{0001F60A}".utf16.count)
}

NSStringAPIs.test("lengthOfBytesUsingEncoding(_:)") {
  expectEqual(1, "a".lengthOfBytes(using: .utf8))
  expectEqual(2, "あ".lengthOfBytes(using: .shiftJIS))
}

NSStringAPIs.test("lineRangeFor(_:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = s.index(s.startIndex, offsetBy: 16)..<s.index(s.startIndex, offsetBy: 35)
  do {
    let result = s.lineRange(for: r)
    expectEqual("штеко будланула\nбокра и кудрячит\n", s[result])
  }
}

NSStringAPIs.test("linguisticTagsIn(_:scheme:options:orthography:tokenRanges:)") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = s.index(s.startIndex, offsetBy: 5)
  let endIndex = s.index(s.startIndex, offsetBy: 17)
  var tokenRanges: [Range<String.Index>] = []
  var tags = s.linguisticTags(in: startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: [],
      orthography: nil, tokenRanges: &tokenRanges)
  expectEqual(
    [NSLinguisticTagWord, NSLinguisticTagWhitespace, NSLinguisticTagWord],
    tags)
  expectEqual(["Глокая", " ", "куздра"],
      tokenRanges.map { s[$0] } )
}

NSStringAPIs.test("localizedCaseInsensitiveCompare(_:)") {
  expectEqual(ComparisonResult.orderedSame,
      "abCD".localizedCaseInsensitiveCompare("AbCd"))
  expectEqual(ComparisonResult.orderedAscending,
      "abCD".localizedCaseInsensitiveCompare("AbCdE"))

  expectEqual(ComparisonResult.orderedSame,
      "абвг".localizedCaseInsensitiveCompare("АбВг"))
  expectEqual(ComparisonResult.orderedAscending,
      "абВГ".localizedCaseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("localizedCompare(_:)") {
  expectEqual(ComparisonResult.orderedAscending,
      "abCD".localizedCompare("AbCd"))

  expectEqual(ComparisonResult.orderedAscending,
      "абвг".localizedCompare("АбВг"))
}

NSStringAPIs.test("localizedStandardCompare(_:)") {
  expectEqual(ComparisonResult.orderedAscending,
      "abCD".localizedStandardCompare("AbCd"))

  expectEqual(ComparisonResult.orderedAscending,
      "абвг".localizedStandardCompare("АбВг"))
}

NSStringAPIs.test("localizedLowercase") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("abcd", "abCD".localizedLowercase)
    }

    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("абвг", "абВГ".localizedLowercase)
    }
    withOverriddenLocaleCurrentLocale("ru") {
      expectEqual("абвг", "абВГ".localizedLowercase)
    }

    withOverriddenLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedLowercase)
    }

    //
    // Special casing.
    //

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("\u{0069}\u{0307}", "\u{0130}".localizedLowercase)
    }

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0130}".localizedLowercase)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual(
        "\u{0069}\u{0307}",
        "\u{0049}\u{0307}".localizedLowercase)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0049}\u{0307}".localizedLowercase)
    }
  }
}

NSStringAPIs.test("lowercased(with:)") {
  expectLocalizedEquality("abcd", { loc in "abCD".lowercased(with: loc) }, "en")

  expectLocalizedEquality("абвг", { loc in "абВГ".lowercased(with: loc) }, "en")
  expectLocalizedEquality("абвг", { loc in "абВГ".lowercased(with: loc) }, "ru")

  expectLocalizedEquality("たちつてと", { loc in "たちつてと".lowercased(with: loc) }, "ru")

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", { loc in "\u{0130}".lowercased(with: loc) }, "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", { loc in "\u{0130}".lowercased(with: loc) }, "tr")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", { loc in "\u{0049}\u{0307}".lowercased(with: loc) }, "en")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", { loc in "\u{0049}\u{0307}".lowercased(with: loc) }, "tr")
}

NSStringAPIs.test("maximumLengthOfBytesUsingEncoding(_:)") {
  do {
    let s = "abc"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytes(using: .utf8))
  }
  do {
    let s = "abc абв"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytes(using: .utf8))
  }
  do {
    let s = "\u{1F60A}"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytes(using: .utf8))
  }
}

NSStringAPIs.test("paragraphRangeFor(_:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = s.index(s.startIndex, offsetBy: 16)..<s.index(s.startIndex, offsetBy: 35)
  do {
    let result = s.paragraphRange(for: r)
    expectEqual("штеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n", s[result])
  }
}

NSStringAPIs.test("pathComponents") {
  expectEqual([ "/", "foo", "bar" ] as [NSString], ("/foo/bar" as NSString).pathComponents as [NSString])
  expectEqual([ "/", "абв", "где" ] as [NSString], ("/абв/где" as NSString).pathComponents as [NSString])
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
  expectEqual(["foo", "bar"],
      "(\"foo\", \"bar\")".propertyList() as! [String])
}

NSStringAPIs.test("propertyListFromStringsFileFormat()") {
  expectEqual(["foo": "bar", "baz": "baz"],
      "/* comment */\n\"foo\" = \"bar\";\n\"baz\";"
          .propertyListFromStringsFileFormat() as Dictionary<String, String>)
}

NSStringAPIs.test("rangeOfCharacterFrom(_:options:range:)") {
  do {
    let charset = CharacterSet(charactersIn: "абв")
    do {
      let s = "Глокая куздра"
      let r = s.rangeOfCharacter(from: charset)!
      expectEqual(s.index(s.startIndex, offsetBy: 4), r.lowerBound)
      expectEqual(s.index(s.startIndex, offsetBy: 5), r.upperBound)
    }
    do {
      expectNil("клмн".rangeOfCharacter(from: charset))
    }
    do {
      let s = "абвклмнабвклмн"
      let r = s.rangeOfCharacter(from: charset,
          options: .backwards)!
      expectEqual(s.index(s.startIndex, offsetBy: 9), r.lowerBound)
      expectEqual(s.index(s.startIndex, offsetBy: 10), r.upperBound)
    }
    do {
      let s = "абвклмнабв"
      let r = s.rangeOfCharacter(from: charset,
          range: s.index(s.startIndex, offsetBy: 3)..<s.endIndex)!
      expectEqual(s.index(s.startIndex, offsetBy: 7), r.lowerBound)
      expectEqual(s.index(s.startIndex, offsetBy: 8), r.upperBound)
    }
  }

  do {
    let charset = CharacterSet(charactersIn: "\u{305f}\u{3099}")
    expectNil("\u{3060}".rangeOfCharacter(from: charset))
  }
  do {
    let charset = CharacterSet(charactersIn: "\u{3060}")
    expectNil("\u{305f}\u{3099}".rangeOfCharacter(from: charset))
  }

  do {
    let charset = CharacterSet(charactersIn: "\u{1F600}")
    do {
      let s = "abc\u{1F600}"
      expectEqual("\u{1F600}",
          s[s.rangeOfCharacter(from: charset)!])
    }
    do {
      expectNil("abc\u{1F601}".rangeOfCharacter(from: charset))
    }
  }
}

NSStringAPIs.test("rangeOfComposedCharacterSequence(at:)") {
  let s = "\u{1F601}abc \u{305f}\u{3099} def"
  expectEqual("\u{1F601}", s[s.rangeOfComposedCharacterSequence(
      at: s.startIndex)])
  expectEqual("a", s[s.rangeOfComposedCharacterSequence(
      at: s.index(s.startIndex, offsetBy: 1))])
  expectEqual("\u{305f}\u{3099}", s[s.rangeOfComposedCharacterSequence(
      at: s.index(s.startIndex, offsetBy: 5))])
  expectEqual(" ", s[s.rangeOfComposedCharacterSequence(
      at: s.index(s.startIndex, offsetBy: 6))])
}

NSStringAPIs.test("rangeOfComposedCharacterSequences(for:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("\u{1F601}a", s[s.rangeOfComposedCharacterSequences(
      for: s.startIndex..<s.index(s.startIndex, offsetBy: 2))])
  expectEqual("せ\u{3099}そ\u{3099}", s[s.rangeOfComposedCharacterSequences(
      for: s.index(s.startIndex, offsetBy: 8)..<s.index(s.startIndex, offsetBy: 10))])
}

func toIntRange(
  _ string: String, _ maybeRange: Range<String.Index>?
) -> Range<Int>? {
  guard let range = maybeRange else { return nil }

  return
    string.distance(from: string.startIndex, to: range.lowerBound) ..<
    string.distance(from: string.startIndex, to: range.upperBound)
}

NSStringAPIs.test("range(of:options:range:locale:)") {
  do {
    let s = ""
    expectNil(s.range(of: ""))
    expectNil(s.range(of: "abc"))
  }
  do {
    let s = "abc"
    expectNil(s.range(of: ""))
    expectNil(s.range(of: "def"))
    expectOptionalEqual(0..<3, toIntRange(s, s.range(of: "abc")))
  }
  do {
    let s = "さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"
    expectOptionalEqual(2..<3, toIntRange(s, s.range(of: "す\u{3099}")))
    expectOptionalEqual(2..<3, toIntRange(s, s.range(of: "\u{305a}")))

    expectNil(s.range(of: "\u{3099}す"))
    expectNil(s.range(of: "す"))

    // Note: here `rangeOf` API produces indexes that don't point between
    // grapheme cluster boundaries -- these cannot be created with public
    // String interface.
    //
    // FIXME: why does this search succeed and the above queries fail?  There is
    // no apparent pattern.
    expectEqual("\u{3099}", s[s.range(of: "\u{3099}")!])
  }
  do {
    let s = "а\u{0301}б\u{0301}в\u{0301}г\u{0301}"
    expectOptionalEqual(0..<1, toIntRange(s, s.range(of: "а\u{0301}")))
    expectOptionalEqual(1..<2, toIntRange(s, s.range(of: "б\u{0301}")))

    expectNil(s.range(of: "б"))
    expectNil(s.range(of: "\u{0301}б"))

    // FIXME: Again, indexes that don't correspond to grapheme
    // cluster boundaries.
    expectEqual("\u{0301}", s[s.range(of: "\u{0301}")!])
  }
}

NSStringAPIs.test("contains(_:)") {
  withOverriddenLocaleCurrentLocale("en") { () -> Void in
    expectFalse("".contains(""))
    expectFalse("".contains("a"))
    expectFalse("a".contains(""))
    expectFalse("a".contains("b"))
    expectTrue("a".contains("a"))
    expectFalse("a".contains("A"))
    expectFalse("A".contains("a"))
    expectFalse("a".contains("a\u{0301}"))
    expectTrue("a\u{0301}".contains("a\u{0301}"))
    expectFalse("a\u{0301}".contains("a"))
    expectTrue("a\u{0301}".contains("\u{0301}"))
    expectFalse("a".contains("\u{0301}"))

    expectFalse("i".contains("I"))
    expectFalse("I".contains("i"))
    expectFalse("\u{0130}".contains("i"))
    expectFalse("i".contains("\u{0130}"))

    return ()
  }

  withOverriddenLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".contains("ı"))
  }
}

NSStringAPIs.test("localizedCaseInsensitiveContains(_:)") {
  withOverriddenLocaleCurrentLocale("en") { () -> Void in
    expectFalse("".localizedCaseInsensitiveContains(""))
    expectFalse("".localizedCaseInsensitiveContains("a"))
    expectFalse("a".localizedCaseInsensitiveContains(""))
    expectFalse("a".localizedCaseInsensitiveContains("b"))
    expectTrue("a".localizedCaseInsensitiveContains("a"))
    expectTrue("a".localizedCaseInsensitiveContains("A"))
    expectTrue("A".localizedCaseInsensitiveContains("a"))
    expectFalse("a".localizedCaseInsensitiveContains("a\u{0301}"))
    expectTrue("a\u{0301}".localizedCaseInsensitiveContains("a\u{0301}"))
    expectFalse("a\u{0301}".localizedCaseInsensitiveContains("a"))
    expectTrue("a\u{0301}".localizedCaseInsensitiveContains("\u{0301}"))
    expectFalse("a".localizedCaseInsensitiveContains("\u{0301}"))

    expectTrue("i".localizedCaseInsensitiveContains("I"))
    expectTrue("I".localizedCaseInsensitiveContains("i"))
    expectFalse("\u{0130}".localizedCaseInsensitiveContains("i"))
    expectFalse("i".localizedCaseInsensitiveContains("\u{0130}"))

    return ()
  }

  withOverriddenLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".localizedCaseInsensitiveContains("ı"))
  }
}

NSStringAPIs.test("localizedStandardContains(_:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenLocaleCurrentLocale("en") { () -> Void in
      expectFalse("".localizedStandardContains(""))
      expectFalse("".localizedStandardContains("a"))
      expectFalse("a".localizedStandardContains(""))
      expectFalse("a".localizedStandardContains("b"))
      expectTrue("a".localizedStandardContains("a"))
      expectTrue("a".localizedStandardContains("A"))
      expectTrue("A".localizedStandardContains("a"))
      expectTrue("a".localizedStandardContains("a\u{0301}"))
      expectTrue("a\u{0301}".localizedStandardContains("a\u{0301}"))
      expectTrue("a\u{0301}".localizedStandardContains("a"))
      expectTrue("a\u{0301}".localizedStandardContains("\u{0301}"))
      expectFalse("a".localizedStandardContains("\u{0301}"))

      expectTrue("i".localizedStandardContains("I"))
      expectTrue("I".localizedStandardContains("i"))
      expectTrue("\u{0130}".localizedStandardContains("i"))
      expectTrue("i".localizedStandardContains("\u{0130}"))

      return ()
    }

    withOverriddenLocaleCurrentLocale("tr") {
      expectTrue("\u{0130}".localizedStandardContains("ı"))
    }
  }
}

NSStringAPIs.test("localizedStandardRange(of:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    func rangeOf(_ string: String, _ substring: String) -> Range<Int>? {
      return toIntRange(
        string, string.localizedStandardRange(of: substring))
    }
    withOverriddenLocaleCurrentLocale("en") { () -> Void in
      expectNil(rangeOf("", ""))
      expectNil(rangeOf("", "a"))
      expectNil(rangeOf("a", ""))
      expectNil(rangeOf("a", "b"))
      expectEqual(0..<1, rangeOf("a", "a"))
      expectEqual(0..<1, rangeOf("a", "A"))
      expectEqual(0..<1, rangeOf("A", "a"))
      expectEqual(0..<1, rangeOf("a", "a\u{0301}"))
      expectEqual(0..<1, rangeOf("a\u{0301}", "a\u{0301}"))
      expectEqual(0..<1, rangeOf("a\u{0301}", "a"))
      do {
        // FIXME: Indices that don't correspond to grapheme cluster boundaries.
        let s = "a\u{0301}"
        expectEqual(
          "\u{0301}", s[s.localizedStandardRange(of: "\u{0301}")!])
      }
      expectNil(rangeOf("a", "\u{0301}"))

      expectEqual(0..<1, rangeOf("i", "I"))
      expectEqual(0..<1, rangeOf("I", "i"))
      expectEqual(0..<1, rangeOf("\u{0130}", "i"))
      expectEqual(0..<1, rangeOf("i", "\u{0130}"))
      return ()
    }

    withOverriddenLocaleCurrentLocale("tr") {
      expectEqual(0..<1, rangeOf("\u{0130}", "ı"))
    }
  }
}

NSStringAPIs.test("smallestEncoding") {
  let availableEncodings: [String.Encoding] = String.availableStringEncodings
  expectTrue(availableEncodings.contains("abc".smallestEncoding))
}

func getHomeDir() -> String {
#if os(OSX)
  return String(cString: getpwuid(getuid()).pointee.pw_dir)
#elseif os(iOS) || os(tvOS) || os(watchOS)
  // getpwuid() returns null in sandboxed apps under iOS simulator.
  return NSHomeDirectory()
#else
  preconditionFailed("implement")
#endif
}

NSStringAPIs.test("addingPercentEncoding(withAllowedCharacters:)") {
  expectOptionalEqual(
    "abcd1234",
    "abcd1234".addingPercentEncoding(withAllowedCharacters: .alphanumerics))
  expectOptionalEqual(
    "abcd%20%D0%B0%D0%B1%D0%B2%D0%B3",
    "abcd абвг".addingPercentEncoding(withAllowedCharacters: .alphanumerics))
}

NSStringAPIs.test("appendingFormat(_:_:...)") {
  expectEqual("", "".appendingFormat(""))
  expectEqual("a", "a".appendingFormat(""))
  expectEqual(
    "abc абв \u{0001F60A}",
    "abc абв \u{0001F60A}".appendingFormat(""))

  let formatArg: NSString = "привет мир \u{0001F60A}"
  expectEqual(
    "abc абв \u{0001F60A}def привет мир \u{0001F60A} 42",
    "abc абв \u{0001F60A}"
      .appendingFormat("def %@ %ld", formatArg, 42))
}

NSStringAPIs.test("appending(_:)") {
  expectEqual("", "".appending(""))
  expectEqual("a", "a".appending(""))
  expectEqual("a", "".appending("a"))
  expectEqual("さ\u{3099}", "さ".appending("\u{3099}"))
}

NSStringAPIs.test("folding(options:locale:)") {

  func fwo(
    _ s: String, _ options: String.CompareOptions
  ) -> (Locale?) -> String {
    return { loc in s.folding(options: options, locale: loc) }
  }
  
  expectLocalizedEquality("abcd", fwo("abCD", .caseInsensitive), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality(
    "\u{0069}\u{0307}", fwo("\u{0130}", .caseInsensitive), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality(
    "\u{0069}", fwo("\u{0130}", .caseInsensitive), "tr")

  expectLocalizedEquality(
    "example123", fwo("ｅｘａｍｐｌｅ１２３", .widthInsensitive), "en")
}

NSStringAPIs.test("padding(toLength:withPad:startingAtIndex:)") {
  expectEqual(
    "abc абв \u{0001F60A}",
    "abc абв \u{0001F60A}".padding(
      toLength: 10, withPad: "XYZ", startingAt: 0))
  expectEqual(
    "abc абв \u{0001F60A}XYZXY",
    "abc абв \u{0001F60A}".padding(
      toLength: 15, withPad: "XYZ", startingAt: 0))
  expectEqual(
    "abc абв \u{0001F60A}YZXYZ",
    "abc абв \u{0001F60A}".padding(
      toLength: 15, withPad: "XYZ", startingAt: 1))
}

NSStringAPIs.test("replacingCharacters(in:with:)") {
  do {
    let empty = ""
    expectEqual("", empty.replacingCharacters(
      in: empty.startIndex..<empty.startIndex, with: ""))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.replacingCharacters(
    in: s.startIndex..<s.startIndex, with: ""))
  expectEqual(s, s.replacingCharacters(
    in: s.endIndex..<s.endIndex, with: ""))
  expectEqual("zzz" + s, s.replacingCharacters(
    in: s.startIndex..<s.startIndex, with: "zzz"))
  expectEqual(s + "zzz", s.replacingCharacters(
    in: s.endIndex..<s.endIndex, with: "zzz"))

  expectEqual(
    "す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.startIndex..<s.index(s.startIndex, offsetBy: 7), with: ""))
  expectEqual(
    "zzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.startIndex..<s.index(s.startIndex, offsetBy: 7), with: "zzz"))
  expectEqual(
    "\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.startIndex..<s.index(s.startIndex, offsetBy: 7), with: "\u{1F602}"))

  expectEqual("\u{1F601}", s.replacingCharacters(
    in: s.index(after: s.startIndex)..<s.endIndex, with: ""))
  expectEqual("\u{1F601}zzz", s.replacingCharacters(
    in: s.index(after: s.startIndex)..<s.endIndex, with: "zzz"))
  expectEqual("\u{1F601}\u{1F602}", s.replacingCharacters(
    in: s.index(after: s.startIndex)..<s.endIndex, with: "\u{1F602}"))

  expectEqual(
    "\u{1F601}aす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.index(s.startIndex, offsetBy: 2)..<s.index(s.startIndex, offsetBy: 7), with: ""))
  expectEqual(
    "\u{1F601}azzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.index(s.startIndex, offsetBy: 2)..<s.index(s.startIndex, offsetBy: 7), with: "zzz"))
  expectEqual(
    "\u{1F601}a\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharacters(
      in: s.index(s.startIndex, offsetBy: 2)..<s.index(s.startIndex, offsetBy: 7),
      with: "\u{1F602}"))
}

NSStringAPIs.test("replacingOccurrences(of:with:options:range:)") {
  do {
    let empty = ""
    expectEqual("", empty.replacingOccurrences(
      of: "", with: ""))
    expectEqual("", empty.replacingOccurrences(
      of: "", with: "xyz"))
    expectEqual("", empty.replacingOccurrences(
      of: "abc", with: "xyz"))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.replacingOccurrences(of: "", with: "xyz"))
  expectEqual(s, s.replacingOccurrences(of: "xyz", with: ""))

  expectEqual("", s.replacingOccurrences(of: s, with: ""))

  expectEqual(
    "\u{1F601}xyzbc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(of: "a", with: "xyz"))

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "\u{1F601}", with: "\u{1F602}\u{1F603}"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "し\u{3099}", with: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "し\u{3099}", with: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "\u{3058}", with: "xyz"))

  //
  // Use non-default 'options:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "\u{1F601}", with: "\u{1F602}\u{1F603}",
      options: String.CompareOptions.literal))

  expectEqual(s, s.replacingOccurrences(
    of: "\u{3058}", with: "xyz",
    options: String.CompareOptions.literal))

  //
  // Use non-default 'range:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrences(
      of: "\u{1F601}", with: "\u{1F602}\u{1F603}",
      options: String.CompareOptions.literal,
      range: s.startIndex..<s.index(s.startIndex, offsetBy: 1)))

  expectEqual(s, s.replacingOccurrences(
      of: "\u{1F601}", with: "\u{1F602}\u{1F603}",
      options: String.CompareOptions.literal,
      range: s.index(s.startIndex, offsetBy: 1)..<s.index(s.startIndex, offsetBy: 3)))
}

NSStringAPIs.test("removingPercentEncoding") {
  expectOptionalEqual(
    "abcd абвг",
    "abcd абвг".removingPercentEncoding)

  expectOptionalEqual(
    "abcd абвг\u{0000}\u{0001}",
    "abcd абвг%00%01".removingPercentEncoding)

  expectOptionalEqual(
    "abcd абвг",
    "%61%62%63%64%20%D0%B0%D0%B1%D0%B2%D0%B3".removingPercentEncoding)

  expectOptionalEqual(
    "abcd абвг",
    "ab%63d %D0%B0%D0%B1%D0%B2%D0%B3".removingPercentEncoding)

  expectNil("%ED%B0".removingPercentEncoding)

  expectNil("%zz".removingPercentEncoding)

  expectNil("abcd%FF".removingPercentEncoding)

  expectNil("%".removingPercentEncoding)
}

NSStringAPIs.test("removingPercentEncoding/OSX 10.9")
  .xfail(.osxMinor(10, 9, reason: "looks like a bug in Foundation in OS X 10.9"))
  .xfail(.iOSMajor(7, reason: "same bug in Foundation in iOS 7.*"))
  .skip(.iOSSimulatorAny("same bug in Foundation in iOS Simulator 7.*"))
  .code {
  expectOptionalEqual("", "".removingPercentEncoding)
}

NSStringAPIs.test("trimmingCharacters(in:)") {
  expectEqual("", "".trimmingCharacters(
    in: CharacterSet.decimalDigits))

  expectEqual("abc", "abc".trimmingCharacters(
    in: CharacterSet.decimalDigits))

  expectEqual("", "123".trimmingCharacters(
    in: CharacterSet.decimalDigits))

  expectEqual("abc", "123abc789".trimmingCharacters(
    in: CharacterSet.decimalDigits))

  // Performs Unicode scalar comparison.
  expectEqual(
    "し\u{3099}abc",
    "し\u{3099}abc".trimmingCharacters(
      in: CharacterSet(charactersIn: "\u{3058}")))
}

NSStringAPIs.test("NSString.stringsByAppendingPaths(_:)") {
  expectEqual([] as [NSString], ("" as NSString).strings(byAppendingPaths: []) as [NSString])
  expectEqual(
    [ "/tmp/foo", "/tmp/bar" ] as [NSString],
    ("/tmp" as NSString).strings(byAppendingPaths: [ "foo", "bar" ]) as [NSString])
}

NSStringAPIs.test("substring(from:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.substring(from: s.startIndex))
  expectEqual("せ\u{3099}そ\u{3099}",
      s.substring(from: s.index(s.startIndex, offsetBy: 8)))
  expectEqual("", s.substring(from: s.index(s.startIndex, offsetBy: 10)))
}

NSStringAPIs.test("substring(to:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substring(to: s.startIndex))
  expectEqual("\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}",
      s.substring(to: s.index(s.startIndex, offsetBy: 8)))
  expectEqual(s, s.substring(to: s.index(s.startIndex, offsetBy: 10)))
}

NSStringAPIs.test("substring(with:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substring(with: s.startIndex..<s.startIndex))
  expectEqual(
    "",
    s.substring(with: s.index(s.startIndex, offsetBy: 1)..<s.index(s.startIndex, offsetBy: 1)))
  expectEqual("", s.substring(with: s.endIndex..<s.endIndex))
  expectEqual(s, s.substring(with: s.startIndex..<s.endIndex))
  expectEqual(
    "さ\u{3099}し\u{3099}す\u{3099}",
    s.substring(with: s.index(s.startIndex, offsetBy: 5)..<s.index(s.startIndex, offsetBy: 8)))
}

NSStringAPIs.test("localizedUppercase") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("ABCD", "abCD".localizedUppercase)
    }

    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("АБВГ", "абВГ".localizedUppercase)
    }

    withOverriddenLocaleCurrentLocale("ru") {
      expectEqual("АБВГ", "абВГ".localizedUppercase)
    }

    withOverriddenLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedUppercase)
    }

    //
    // Special casing.
    //

    // U+0069 LATIN SMALL LETTER I
    // to upper case:
    // U+0049 LATIN CAPITAL LETTER I
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("\u{0049}", "\u{0069}".localizedUppercase)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenLocaleCurrentLocale("tr") {
      expectEqual("\u{0130}", "\u{0069}".localizedUppercase)
    }

    // U+00DF LATIN SMALL LETTER SHARP S
    // to upper case:
    // U+0053 LATIN CAPITAL LETTER S
    // U+0073 LATIN SMALL LETTER S
    // But because the whole string is converted to uppercase, we just get two
    // U+0053.
    withOverriddenLocaleCurrentLocale("en") {
      expectEqual("\u{0053}\u{0053}", "\u{00df}".localizedUppercase)
    }

    // U+FB01 LATIN SMALL LIGATURE FI
    // to upper case:
    // U+0046 LATIN CAPITAL LETTER F
    // U+0069 LATIN SMALL LETTER I
    // But because the whole string is converted to uppercase, we get U+0049
    // LATIN CAPITAL LETTER I.
    withOverriddenLocaleCurrentLocale("ru") {
      expectEqual("\u{0046}\u{0049}", "\u{fb01}".localizedUppercase)
    }
  }
}

NSStringAPIs.test("uppercased(with:)") {
  expectLocalizedEquality("ABCD", { loc in "abCD".uppercased(with: loc) }, "en")

  expectLocalizedEquality("АБВГ", { loc in "абВГ".uppercased(with: loc) }, "en")
  expectLocalizedEquality("АБВГ", { loc in "абВГ".uppercased(with: loc) }, "ru")

  expectLocalizedEquality("たちつてと", { loc in "たちつてと".uppercased(with: loc) }, "ru")

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality("\u{0049}", { loc in "\u{0069}".uppercased(with: loc) }, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality("\u{0130}", { loc in "\u{0069}".uppercased(with: loc) }, "tr")

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectLocalizedEquality("\u{0053}\u{0053}", { loc in "\u{00df}".uppercased(with: loc) }, "en")

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectLocalizedEquality("\u{0046}\u{0049}", { loc in "\u{fb01}".uppercased(with: loc) }, "ru")
}

NSStringAPIs.test("write(toFile:atomically:encoding:error:)") {
  let (_, nonExistentPath) = createNSStringTemporaryFile()
  do {
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    try s.write(
      toFile: nonExistentPath, atomically: false, encoding: .ascii)

    let content = try String(
      contentsOfFile: nonExistentPath, encoding: .ascii)

    expectEqual(s, content)
  } catch {
    expectUnreachableCatch(error)
  }
}

NSStringAPIs.test("write(to:atomically:encoding:error:)") {
  let (_, nonExistentPath) = createNSStringTemporaryFile()
  let nonExistentURL = URL(string: "file://" + nonExistentPath)!
  do {
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    try s.write(
      to: nonExistentURL, atomically: false, encoding: .ascii)

    let content = try String(
      contentsOfFile: nonExistentPath, encoding: .ascii)

    expectEqual(s, content)
  } catch {
    expectUnreachableCatch(error)
  }
}

NSStringAPIs.test("applyingTransform(_:reverse:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    do {
      let source = "tre\u{300}s k\u{fc}hl"
      expectEqual(
        "tres kuhl",
        source.applyingTransform(.stripDiacritics, reverse: false))
    }
    do {
      let source = "hiragana"
      expectEqual(
        "ひらがな",
        source.applyingTransform(.latinToHiragana, reverse: false))
    }
    do {
      let source = "ひらがな"
      expectEqual(
        "hiragana",
        source.applyingTransform(.latinToHiragana, reverse: true))
    }
  }
}

NSStringAPIs.test("SameTypeComparisons") {
  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  let xs = "\u{1e69}"
  expectTrue(xs == "s\u{323}\u{307}")
  expectFalse(xs != "s\u{323}\u{307}")
  expectTrue("s\u{323}\u{307}" == xs)
  expectFalse("s\u{323}\u{307}" != xs)
  expectTrue("\u{1e69}" == "s\u{323}\u{307}")
  expectFalse("\u{1e69}" != "s\u{323}\u{307}")
  expectTrue(xs == xs)
  expectFalse(xs != xs)
}

NSStringAPIs.test("MixedTypeComparisons") {
  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  // NSString does not decompose characters, so the two strings will be (==) in
  // swift but not in Foundation.
  let xs = "\u{1e69}"
  let ys: NSString = "s\u{323}\u{307}"
  expectFalse(ys == "\u{1e69}")
  expectTrue(ys != "\u{1e69}")
  expectFalse("\u{1e69}" == ys)
  expectTrue("\u{1e69}" != ys)
  expectFalse(xs as NSString == ys)
  expectTrue(xs as NSString != ys)
  expectTrue(ys == ys)
  expectFalse(ys != ys)
}

NSStringAPIs.test("CompareStringsWithUnpairedSurrogates")
  .xfail(
    .custom({ true },
    reason: "<rdar://problem/18029104> Strings referring to underlying " +
      "storage with unpaired surrogates compare unequal"))
  .code {
  let donor = "abcdef"
  let acceptor = "\u{1f601}\u{1f602}\u{1f603}"

  expectEqual("\u{fffd}\u{1f602}\u{fffd}",
    acceptor[donor.index(donor.startIndex, offsetBy: 1)..<donor.index(donor.startIndex, offsetBy: 5)])
}

NSStringAPIs.test("copy construction") {
  let expected = "abcd"
  let x = NSString(string: expected as NSString)
  expectEqual(expected, x as String)
  let y = NSMutableString(string: expected as NSString)
  expectEqual(expected, y as String)
}

runAllTests()

