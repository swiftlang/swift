// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out
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

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    // Ensure that copying this string produces a class that CoreFoundation
    // does not know about.
    return self
  }

  @objc override var length: Int {
    return _value.count
  }

  @objc override func characterAtIndex(index: Int) -> unichar {
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
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
  expectNotEqual(0, availableEncodings.count)

  let defaultCStringEncoding = String.defaultCStringEncoding()
  expectTrue(availableEncodings.contains(defaultCStringEncoding))

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

  withOverriddenNSLocaleCurrentLocale("en_US") {
    expectEqual("0.5", String.localizedStringWithFormat("%g", 0.5))
  }

  withOverriddenNSLocaleCurrentLocale("uk") {
    expectEqual("0,5", String.localizedStringWithFormat("%g", 0.5))
  }
}

NSStringAPIs.test("init(contentsOfFile:encoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()

  do {
    let content = try String(
      contentsOfFile: existingPath, encoding: NSASCIIStringEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  do {
    let content = try String(
      contentsOfFile: nonExistentPath, encoding: NSASCIIStringEncoding)
    expectUnreachable()
  } catch {
  }
}

NSStringAPIs.test("init(contentsOfFile:usedEncoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()

  do {
    var usedEncoding: NSStringEncoding = 0
    let content = try String(
      contentsOfFile: existingPath, usedEncoding: &usedEncoding)
    expectNotEqual(0, usedEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  var usedEncoding: NSStringEncoding = 0
  do {
    _ = try String(contentsOfFile: nonExistentPath)
    expectUnreachable()
  } catch {
    expectEqual(0, usedEncoding)
  }
}


NSStringAPIs.test("init(contentsOfURL:encoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = NSURL(string: "file://" + existingPath)!
  let nonExistentURL = NSURL(string: "file://" + nonExistentPath)!
  do {
    let content = try String(
      contentsOfURL: existingURL, encoding: NSASCIIStringEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  do {
    _ = try String(contentsOfURL: nonExistentURL, encoding: NSASCIIStringEncoding)
    expectUnreachable()
  } catch {
  }
}

NSStringAPIs.test("init(contentsOfURL:usedEncoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = NSURL(string: "file://" + existingPath)!
  let nonExistentURL = NSURL(string: "file://" + nonExistentPath)!
  do {
    var usedEncoding: NSStringEncoding = 0
    let content = try String(
      contentsOfURL: existingURL, usedEncoding: &usedEncoding)

    expectNotEqual(0, usedEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  var usedEncoding: NSStringEncoding = 0
  do {
    _ = try String(contentsOfURL: nonExistentURL, usedEncoding: &usedEncoding)
    expectUnreachable()
  } catch {
    expectEqual(0, usedEncoding)
  }
}

NSStringAPIs.test("init(withCString_:encoding:)") {
  expectOptionalEqual("foo, a basmati bar!",
      String(CString: 
          "foo, a basmati bar!", encoding: String.defaultCStringEncoding()))
}

NSStringAPIs.test("init(UTF8String:)") {
  var s = "foo あいう"
  var up = UnsafeMutablePointer<UInt8>.alloc(100)
  var i = 0
  for b in s.utf8 {
    up[i] = b
    i += 1
  }
  up[i] = 0
  expectOptionalEqual(s, String(UTF8String: UnsafePointer(up)))
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

NSStringAPIs.test("localizedCapitalizedString") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
      expectEqual(
        "Foo Foo Foo Foo",
        "foo Foo fOO FOO".localizedCapitalizedString)
      expectEqual("Жжж", "жжж".localizedCapitalizedString)
      return ()
    }

    //
    // Special casing.
    //

    // U+0069 LATIN SMALL LETTER I
    // to upper case:
    // U+0049 LATIN CAPITAL LETTER I
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("Iii Iii", "iii III".localizedCapitalizedString)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0130}ii Iıı", "iii III".localizedCapitalizedString)
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
  expected: String,
  _ op: (_: NSLocale?) -> String,
  _ localeID: String? = nil,
  @autoclosure _ message: () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),  
  file: String = __FILE__, line: UInt = __LINE__
) {
  let trace = stackTrace.pushIf(showFrame, file: file, line: line)

  let locale = localeID.map {
    NSLocale(localeIdentifier: $0)
  } ?? NSLocale.currentLocale()
  
  expectEqual(
    expected, op(locale),
    message(), stackTrace: trace)
  
  expectEqual(
    op(NSLocale.systemLocale()), op(nil),
    message(), stackTrace: trace)
}

NSStringAPIs.test("capitalizedStringWithLocale(_:)") {
  expectLocalizedEquality(
    "Foo Foo Foo Foo",
    "foo Foo fOO FOO".capitalizedStringWithLocale)
  
  expectLocalizedEquality("Жжж","жжж".capitalizedStringWithLocale)

  expectEqual(
    "Foo Foo Foo Foo",
    "foo Foo fOO FOO".capitalizedStringWithLocale(nil))
  expectEqual("Жжж", "жжж".capitalizedStringWithLocale(nil))

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality(
    "Iii Iii",
    "iii III".capitalizedStringWithLocale, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality(
    "İii Iıı",
    "iii III".capitalizedStringWithLocale, "tr")
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
      "abcd".commonPrefixWithString("abdc", options: []))
  expectEqual("abC",
      "abCd".commonPrefixWithString("abce", options: .CaseInsensitiveSearch))

  expectEqual("аб",
      "абвг".commonPrefixWithString("абгв", options: []))
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

  do {
    let s = "abcd"
    let r = s.startIndex.successor()..<s.endIndex
    expectEqual(NSComparisonResult.OrderedSame,
        s.compare("bcd", range: r))
  }
  do {
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
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  do {
    var count = nonExistentPath.completePathIntoString(caseSensitive: false)
    expectEqual(0, count)
  }

  do {
    var outputName = "None Found"
    var count = nonExistentPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = [ "foo", "bar" ]
    var count = nonExistentPath.completePathIntoString(
        &outputName, caseSensitive: false, matchesIntoArray: &outputArray)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
    expectEqual([ "foo", "bar" ], outputArray)
  }

  do {
    var count = existingPath.completePathIntoString(caseSensitive: false)
    expectEqual(1, count)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = [ "foo", "bar" ]
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false, matchesIntoArray: &outputArray)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
    expectEqual([ existingPath ], outputArray)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false, filterTypes: [ "txt" ])

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }
}

NSStringAPIs.test("componentsSeparatedByCharactersInSet(_:)") {
  expectEqual([ "" ], "".componentsSeparatedByCharactersInSet(
    NSCharacterSet.decimalDigitCharacterSet()))

  expectEqual(
    [ "абв", "", "あいう", "abc" ],
    "абв12あいう3abc".componentsSeparatedByCharactersInSet(
        NSCharacterSet.decimalDigitCharacterSet()))

  expectEqual(
    [ "абв", "", "あいう", "abc" ],
    "абв\u{1F601}\u{1F602}あいう\u{1F603}abc"
      .componentsSeparatedByCharactersInSet(
        NSCharacterSet(charactersInString: "\u{1F601}\u{1F602}\u{1F603}")))

  // Performs Unicode scalar comparison.
  expectEqual(
    [ "abcし\u{3099}def" ],
    "abcし\u{3099}def".componentsSeparatedByCharactersInSet(
      NSCharacterSet(charactersInString: "\u{3058}")))
}

NSStringAPIs.test("componentsSeparatedByString(_:)") {
  expectEqual([ "" ], "".componentsSeparatedByString("//"))

  expectEqual(
    [ "абв", "あいう", "abc" ],
    "абв//あいう//abc".componentsSeparatedByString("//"))

  // Performs normalization.
  expectEqual(
    [ "abc", "def" ],
    "abcし\u{3099}def".componentsSeparatedByString("\u{3058}"))
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

  do {
    let data = "あいう".dataUsingEncoding(NSUTF8StringEncoding)
    let bytes = Array(
      UnsafeBufferPointer(
        start: UnsafePointer<UInt8>(data!.bytes), count: data!.length))
    let expectedBytes: [UInt8] = [
      0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86
    ]
    expectEqualSequence(expectedBytes, bytes)
  }
}

NSStringAPIs.test("initWithData(_:encoding:)") {
  let bytes: [UInt8] = [0xe3, 0x81, 0x82, 0xe3, 0x81, 0x84, 0xe3, 0x81, 0x86]
  let data = NSData(bytes: bytes, length: bytes.count)
  
  expectEmpty(String(data: data, encoding: NSNonLossyASCIIStringEncoding))
  
  expectEqualSequence(
    "あいう".characters, 
    String(data: data, encoding: NSUTF8StringEncoding)!.characters)
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
  let startIndex = s.startIndex.advancedBy(5)
  let endIndex = s.startIndex.advancedBy(62)
  var tags: [String] = []
  var tokens: [String] = []
  var sentences: [String] = []
  s.enumerateLinguisticTagsInRange(startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: [],
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
  let startIndex = s.startIndex.advancedBy(1)
  let endIndex = s.startIndex.advancedBy(5)
  do {
    var substrings: [String] = []
    s.enumerateSubstringsInRange(startIndex..<endIndex,
      options: NSStringEnumerationOptions.ByComposedCharacterSequences) {
      (substring: String?, substringRange: Range<String.Index>,
       enclosingRange: Range<String.Index>, inout stop: Bool)
    in
      substrings.append(substring!)
      expectEqual(substring, s[substringRange])
      expectEqual(substring, s[enclosingRange])
    }
    expectEqual([ "\u{304b}\u{3099}", "お", "☺️", "😀" ], substrings)
  }
  do {
    var substrings: [String] = []
    s.enumerateSubstringsInRange(startIndex..<endIndex,
      options: [.ByComposedCharacterSequences, .SubstringNotRequired]) {
      (substring_: String?, substringRange: Range<String.Index>,
       enclosingRange: Range<String.Index>, inout stop: Bool)
    in
      expectEmpty(substring_)
      let substring = s[substringRange]
      substrings.append(substring)
      expectEqual(substring, s[enclosingRange])
    }
    expectEqual([ "\u{304b}\u{3099}", "お", "☺️", "😀" ], substrings)
  }
}

NSStringAPIs.test("fastestEncoding") {
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
  expectTrue(availableEncodings.contains("abc".fastestEncoding))
}

NSStringAPIs.test("getBytes(_:maxLength:usedLength:encoding:options:range:remainingRange:)") {
  let s = "abc абв def где gh жз zzz"
  let startIndex = s.startIndex.advancedBy(8)
  let endIndex = s.startIndex.advancedBy(22)
  do {
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
        options: [],
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(11, usedLength)
    expectEqual(remainingRange.startIndex, startIndex.advancedBy(8))
    expectEqual(remainingRange.endIndex, endIndex)
  }
  do {
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
        options: [],
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(4, usedLength)
    expectEqual(remainingRange.startIndex, startIndex.advancedBy(4))
    expectEqual(remainingRange.endIndex, endIndex)
  }
  do {
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
        options: [],
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(19, usedLength)
    expectEqual(remainingRange.startIndex, endIndex)
    expectEqual(remainingRange.endIndex, endIndex)
  }
  do {
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
        options: [],
        range: startIndex..<endIndex, remainingRange: &remainingRange)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
    expectEqual(4, usedLength)
    expectEqual(remainingRange.startIndex, startIndex.advancedBy(4))
    expectEqual(remainingRange.endIndex, endIndex)
  }
}

NSStringAPIs.test("getCString(_:maxLength:encoding:)") {
  var s = "abc あかさた"
  do {
    // The largest buffer that cannot accommodate the string plus null terminator.
    let bufferLength = 16
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    let result = s.getCString(&buffer, maxLength: 100,
      encoding: NSUTF8StringEncoding)
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
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    let result = s.getCString(&buffer, maxLength: 100,
      encoding: NSUTF8StringEncoding)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
  }
  do {
    // Limit buffer size with 'maxLength'.
    let bufferLength = 100
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    let result = s.getCString(&buffer, maxLength: 8,
      encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
  do {
    // String with unpaired surrogates.
    let illFormedUTF16 = NonContiguousNSString([ 0xd800 ]) as String
    let bufferLength = 100
    var buffer = Array(
      count: bufferLength, repeatedValue: CChar(bitPattern: 0xff))
    let result = illFormedUTF16.getCString(&buffer, maxLength: 100,
      encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
}

NSStringAPIs.test("getLineStart(_:end:contentsEnd:forRange:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
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
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
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

NSStringAPIs.test("init(bytes:encoding:)") {
  var s: String = "abc あかさた"
  expectOptionalEqual(
    s, String(bytes: s.utf8, encoding: NSUTF8StringEncoding))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectEmpty(String(bytes: bytes, length: bytes.count,
      encoding: NSASCIIStringEncoding))
  */

  // FIXME: add a test where this function actually returns nil.
}

NSStringAPIs.test("init(bytesNoCopy:length:encoding:freeWhenDone:)") {
  var s: String = "abc あかさた"
  var bytes: [UInt8] = Array(s.utf8)
  expectOptionalEqual(s, String(bytesNoCopy: &bytes,
      length: bytes.count, encoding: NSUTF8StringEncoding,
      freeWhenDone: false))

  /*
  FIXME: Test disabled because the NSString documentation is unclear about
  what should actually happen in this case.

  expectEmpty(String(bytesNoCopy: &bytes, length: bytes.count,
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
  expectEqual(1, "a".utf16.count)
  expectEqual(2, "\u{0001F60A}".utf16.count)
}

NSStringAPIs.test("lengthOfBytesUsingEncoding(_:)") {
  expectEqual(1, "a".lengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  expectEqual(2, "あ".lengthOfBytesUsingEncoding(NSShiftJISStringEncoding))
}

NSStringAPIs.test("lineRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
    let result = s.lineRangeForRange(r)
    expectEqual("штеко будланула\nбокра и кудрячит\n", s[result])
  }
}

NSStringAPIs.test("linguisticTagsInRange(_:scheme:options:orthography:tokenRanges:)") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = s.startIndex.advancedBy(5)
  let endIndex = s.startIndex.advancedBy(17)
  var tokenRanges: [Range<String.Index>] = []
  var tags = s.linguisticTagsInRange(startIndex..<endIndex,
      scheme: NSLinguisticTagSchemeTokenType,
      options: [],
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

NSStringAPIs.test("localizedLowercaseString") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("abcd", "abCD".localizedLowercaseString)
    }

    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("абвг", "абВГ".localizedLowercaseString)
    }
    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("абвг", "абВГ".localizedLowercaseString)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedLowercaseString)
    }

    //
    // Special casing.
    //

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0069}\u{0307}", "\u{0130}".localizedLowercaseString)
    }

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0130}".localizedLowercaseString)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual(
        "\u{0069}\u{0307}",
        "\u{0049}\u{0307}".localizedLowercaseString)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0049}\u{0307}".localizedLowercaseString)
    }
  }
}

NSStringAPIs.test("lowercaseStringWithLocale(_:)") {
  expectLocalizedEquality("abcd", "abCD".lowercaseStringWithLocale, "en")

  expectLocalizedEquality("абвг", "абВГ".lowercaseStringWithLocale, "en")
  expectLocalizedEquality("абвг", "абВГ".lowercaseStringWithLocale, "ru")

  expectLocalizedEquality("たちつてと", "たちつてと".lowercaseStringWithLocale, "ru")

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", "\u{0130}".lowercaseStringWithLocale, "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", "\u{0130}".lowercaseStringWithLocale, "tr")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercaseStringWithLocale, "en")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", "\u{0049}\u{0307}".lowercaseStringWithLocale, "tr")
}

NSStringAPIs.test("maximumLengthOfBytesUsingEncoding(_:)") {
  do {
    let s = "abc"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
  do {
    let s = "abc абв"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
  do {
    let s = "\u{1F60A}"
    expectLE(s.utf8.count,
        s.maximumLengthOfBytesUsingEncoding(NSUTF8StringEncoding))
  }
}

NSStringAPIs.test("paragraphRangeForRange(_:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
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
      "(\"foo\", \"bar\")".propertyList() as! [String])
}

NSStringAPIs.test("propertyListFromStringsFileFormat()") {
  expectEqual([ "foo": "bar", "baz": "baz" ],
      "/* comment */\n\"foo\" = \"bar\";\n\"baz\";"
          .propertyListFromStringsFileFormat() as Dictionary<String, String>)
}

NSStringAPIs.test("rangeOfCharacterFromSet(_:options:range:)") {
  do {
    let charset = NSCharacterSet(charactersInString: "абв")
    do {
      let s = "Глокая куздра"
      let r = s.rangeOfCharacterFromSet(charset)!
      expectEqual(s.startIndex.advancedBy(4), r.startIndex)
      expectEqual(s.startIndex.advancedBy(5), r.endIndex)
    }
    do {
      expectEmpty("клмн".rangeOfCharacterFromSet(charset))
    }
    do {
      let s = "абвклмнабвклмн"
      let r = s.rangeOfCharacterFromSet(charset,
          options: .BackwardsSearch)!
      expectEqual(s.startIndex.advancedBy(9), r.startIndex)
      expectEqual(s.startIndex.advancedBy(10), r.endIndex)
    }
    do {
      let s = "абвклмнабв"
      let r = s.rangeOfCharacterFromSet(charset,
          range: s.startIndex.advancedBy(3)..<s.endIndex)!
      expectEqual(s.startIndex.advancedBy(7), r.startIndex)
      expectEqual(s.startIndex.advancedBy(8), r.endIndex)
    }
  }

  do {
    let charset = NSCharacterSet(charactersInString: "\u{305f}\u{3099}")
    expectEmpty("\u{3060}".rangeOfCharacterFromSet(charset))
  }
  do {
    let charset = NSCharacterSet(charactersInString: "\u{3060}")
    expectEmpty("\u{305f}\u{3099}".rangeOfCharacterFromSet(charset))
  }

  do {
    let charset = NSCharacterSet(charactersInString: "\u{1F600}")
    do {
      let s = "abc\u{1F600}"
      expectEqual("\u{1F600}",
          s[s.rangeOfCharacterFromSet(charset)!])
    }
    do {
      expectEmpty("abc\u{1F601}".rangeOfCharacterFromSet(charset))
    }
  }
}

NSStringAPIs.test("rangeOfComposedCharacterSequenceAtIndex(_:)") {
  let s = "\u{1F601}abc \u{305f}\u{3099} def"
  expectEqual("\u{1F601}", s[s.rangeOfComposedCharacterSequenceAtIndex(
      s.startIndex)])
  expectEqual("a", s[s.rangeOfComposedCharacterSequenceAtIndex(
      s.startIndex.advancedBy(1))])
  expectEqual("\u{305f}\u{3099}", s[s.rangeOfComposedCharacterSequenceAtIndex(
      s.startIndex.advancedBy(5))])
  expectEqual(" ", s[s.rangeOfComposedCharacterSequenceAtIndex(
      s.startIndex.advancedBy(6))])
}

NSStringAPIs.test("rangeOfComposedCharacterSequencesForRange(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("\u{1F601}a", s[s.rangeOfComposedCharacterSequencesForRange(
      s.startIndex..<s.startIndex.advancedBy(2))])
  expectEqual("せ\u{3099}そ\u{3099}", s[s.rangeOfComposedCharacterSequencesForRange(
      s.startIndex.advancedBy(8)..<s.startIndex.advancedBy(10))])
}

func toIntRange(
  string: String, _ maybeRange: Range<String.Index>?
) -> Range<Int>? {
  guard let range = maybeRange else { return nil }

  return
    string.startIndex.distanceTo(range.startIndex) ..<
    string.startIndex.distanceTo(range.endIndex)
}

NSStringAPIs.test("rangeOfString(_:options:range:locale:)") {
  do {
    let s = ""
    expectEmpty(s.rangeOfString(""))
    expectEmpty(s.rangeOfString("abc"))
  }
  do {
    let s = "abc"
    expectEmpty(s.rangeOfString(""))
    expectEmpty(s.rangeOfString("def"))
    expectOptionalEqual(0..<3, toIntRange(s, s.rangeOfString("abc")))
  }
  do {
    let s = "さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"
    expectOptionalEqual(2..<3, toIntRange(s, s.rangeOfString("す\u{3099}")))
    expectOptionalEqual(2..<3, toIntRange(s, s.rangeOfString("\u{305a}")))

    expectEmpty(s.rangeOfString("\u{3099}す"))
    expectEmpty(s.rangeOfString("す"))

    // Note: here `rangeOfString` API produces indexes that don't point between
    // grapheme cluster boundaries -- these cannot be created with public
    // String interface.
    //
    // FIXME: why does this search succeed and the above queries fail?  There is
    // no apparent pattern.
    expectEqual("\u{3099}", s[s.rangeOfString("\u{3099}")!])
  }
  do {
    let s = "а\u{0301}б\u{0301}в\u{0301}г\u{0301}"
    expectOptionalEqual(0..<1, toIntRange(s, s.rangeOfString("а\u{0301}")))
    expectOptionalEqual(1..<2, toIntRange(s, s.rangeOfString("б\u{0301}")))

    expectEmpty(s.rangeOfString("б"))
    expectEmpty(s.rangeOfString("\u{0301}б"))

    // FIXME: Again, indexes that don't correspond to grapheme
    // cluster boundaries.
    expectEqual("\u{0301}", s[s.rangeOfString("\u{0301}")!])
  }
}

NSStringAPIs.test("containsString(_:)") {
  withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
    expectFalse("".containsString(""))
    expectFalse("".containsString("a"))
    expectFalse("a".containsString(""))
    expectFalse("a".containsString("b"))
    expectTrue("a".containsString("a"))
    expectFalse("a".containsString("A"))
    expectFalse("A".containsString("a"))
    expectFalse("a".containsString("a\u{0301}"))
    expectTrue("a\u{0301}".containsString("a\u{0301}"))
    expectFalse("a\u{0301}".containsString("a"))
    expectTrue("a\u{0301}".containsString("\u{0301}"))
    expectFalse("a".containsString("\u{0301}"))

    expectFalse("i".containsString("I"))
    expectFalse("I".containsString("i"))
    expectFalse("\u{0130}".containsString("i"))
    expectFalse("i".containsString("\u{0130}"))

    return ()
  }

  withOverriddenNSLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".containsString("ı"))
  }
}

NSStringAPIs.test("localizedCaseInsensitiveContainsString(_:)") {
  withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
    expectFalse("".localizedCaseInsensitiveContainsString(""))
    expectFalse("".localizedCaseInsensitiveContainsString("a"))
    expectFalse("a".localizedCaseInsensitiveContainsString(""))
    expectFalse("a".localizedCaseInsensitiveContainsString("b"))
    expectTrue("a".localizedCaseInsensitiveContainsString("a"))
    expectTrue("a".localizedCaseInsensitiveContainsString("A"))
    expectTrue("A".localizedCaseInsensitiveContainsString("a"))
    expectFalse("a".localizedCaseInsensitiveContainsString("a\u{0301}"))
    expectTrue("a\u{0301}".localizedCaseInsensitiveContainsString("a\u{0301}"))
    expectFalse("a\u{0301}".localizedCaseInsensitiveContainsString("a"))
    expectTrue("a\u{0301}".localizedCaseInsensitiveContainsString("\u{0301}"))
    expectFalse("a".localizedCaseInsensitiveContainsString("\u{0301}"))

    expectTrue("i".localizedCaseInsensitiveContainsString("I"))
    expectTrue("I".localizedCaseInsensitiveContainsString("i"))
    expectFalse("\u{0130}".localizedCaseInsensitiveContainsString("i"))
    expectFalse("i".localizedCaseInsensitiveContainsString("\u{0130}"))

    return ()
  }

  withOverriddenNSLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".localizedCaseInsensitiveContainsString("ı"))
  }
}

NSStringAPIs.test("localizedStandardContainsString(_:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
      expectFalse("".localizedStandardContainsString(""))
      expectFalse("".localizedStandardContainsString("a"))
      expectFalse("a".localizedStandardContainsString(""))
      expectFalse("a".localizedStandardContainsString("b"))
      expectTrue("a".localizedStandardContainsString("a"))
      expectTrue("a".localizedStandardContainsString("A"))
      expectTrue("A".localizedStandardContainsString("a"))
      expectTrue("a".localizedStandardContainsString("a\u{0301}"))
      expectTrue("a\u{0301}".localizedStandardContainsString("a\u{0301}"))
      expectTrue("a\u{0301}".localizedStandardContainsString("a"))
      expectTrue("a\u{0301}".localizedStandardContainsString("\u{0301}"))
      expectFalse("a".localizedStandardContainsString("\u{0301}"))

      expectTrue("i".localizedStandardContainsString("I"))
      expectTrue("I".localizedStandardContainsString("i"))
      expectTrue("\u{0130}".localizedStandardContainsString("i"))
      expectTrue("i".localizedStandardContainsString("\u{0130}"))

      return ()
    }

    withOverriddenNSLocaleCurrentLocale("tr") {
      expectTrue("\u{0130}".localizedStandardContainsString("ı"))
    }
  }
}

NSStringAPIs.test("localizedStandardRangeOfString(_:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    func rangeOfString(string: String, _ substring: String) -> Range<Int>? {
      return toIntRange(
        string, string.localizedStandardRangeOfString(substring))
    }
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
      expectEmpty(rangeOfString("", ""))
      expectEmpty(rangeOfString("", "a"))
      expectEmpty(rangeOfString("a", ""))
      expectEmpty(rangeOfString("a", "b"))
      expectEqual(0..<1, rangeOfString("a", "a"))
      expectEqual(0..<1, rangeOfString("a", "A"))
      expectEqual(0..<1, rangeOfString("A", "a"))
      expectEqual(0..<1, rangeOfString("a", "a\u{0301}"))
      expectEqual(0..<1, rangeOfString("a\u{0301}", "a\u{0301}"))
      expectEqual(0..<1, rangeOfString("a\u{0301}", "a"))
      do {
        // FIXME: Indices that don't correspond to grapheme cluster boundaries.
        let s = "a\u{0301}"
        expectEqual(
          "\u{0301}", s[s.localizedStandardRangeOfString("\u{0301}")!])
      }
      expectEmpty(rangeOfString("a", "\u{0301}"))

      expectEqual(0..<1, rangeOfString("i", "I"))
      expectEqual(0..<1, rangeOfString("I", "i"))
      expectEqual(0..<1, rangeOfString("\u{0130}", "i"))
      expectEqual(0..<1, rangeOfString("i", "\u{0130}"))
      return ()
    }

    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual(0..<1, rangeOfString("\u{0130}", "ı"))
    }
  }
}

NSStringAPIs.test("smallestEncoding") {
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
  expectTrue(availableEncodings.contains("abc".smallestEncoding))
}

func getHomeDir() -> String {
#if os(OSX)
  return String.fromCString(getpwuid(getuid()).memory.pw_dir)!
#elseif os(iOS) || os(tvOS) || os(watchOS)
  // getpwuid() returns null in sandboxed apps under iOS simulator.
  return NSHomeDirectory()
#else
  preconditionFailed("implement")
#endif
}

NSStringAPIs.test("stringByAddingPercentEscapesUsingEncoding(_:)") {
  expectEmpty(
    "abcd абвг".stringByAddingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))
  expectOptionalEqual("abcd%20%D0%B0%D0%B1%D0%B2%D0%B3",
    "abcd абвг".stringByAddingPercentEscapesUsingEncoding(
      NSUTF8StringEncoding))
}

NSStringAPIs.test("stringByAppendingFormat(_:_:...)") {
  expectEqual("", "".stringByAppendingFormat(""))
  expectEqual("a", "a".stringByAppendingFormat(""))
  expectEqual(
    "abc абв \u{0001F60A}",
    "abc абв \u{0001F60A}".stringByAppendingFormat(""))

  let formatArg: NSString = "привет мир \u{0001F60A}"
  expectEqual(
    "abc абв \u{0001F60A}def привет мир \u{0001F60A} 42",
    "abc абв \u{0001F60A}"
      .stringByAppendingFormat("def %@ %ld", formatArg, 42))
}

NSStringAPIs.test("stringByAppendingPathComponent(_:)") {
  expectEqual("", "".stringByAppendingPathComponent(""))
  expectEqual("a.txt", "".stringByAppendingPathComponent("a.txt"))
  expectEqual("/tmp/a.txt", "/tmp".stringByAppendingPathComponent("a.txt"))
}

NSStringAPIs.test("stringByAppendingString(_:)") {
  expectEqual("", "".stringByAppendingString(""))
  expectEqual("a", "a".stringByAppendingString(""))
  expectEqual("a", "".stringByAppendingString("a"))
  expectEqual("さ\u{3099}", "さ".stringByAppendingString("\u{3099}"))
}

NSStringAPIs.test("stringByDeletingLastPathComponent") {
  expectEqual("", "".stringByDeletingLastPathComponent)
  expectEqual("/", "/".stringByDeletingLastPathComponent)
  expectEqual("/", "/tmp".stringByDeletingLastPathComponent)
  expectEqual("/tmp", "/tmp/a.txt".stringByDeletingLastPathComponent)
}

NSStringAPIs.test("stringByFoldingWithOptions(_:locale:)") {

  func fwo(
    s: String, _ options: NSStringCompareOptions
  ) -> (NSLocale?) -> String {
    return { loc in s.stringByFoldingWithOptions(options, locale: loc) }
  }
  
  expectLocalizedEquality("abcd", fwo("abCD", .CaseInsensitiveSearch), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality(
    "\u{0069}\u{0307}", fwo("\u{0130}", .CaseInsensitiveSearch), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality(
    "\u{0069}", fwo("\u{0130}", .CaseInsensitiveSearch), "tr")

  expectLocalizedEquality(
    "example123", fwo("ｅｘａｍｐｌｅ１２３", .WidthInsensitiveSearch), "en")
}

NSStringAPIs.test("stringByPaddingToLength(_:withString:startingAtIndex:)") {
  expectEqual(
    "abc абв \u{0001F60A}",
    "abc абв \u{0001F60A}".stringByPaddingToLength(
      10, withString: "XYZ", startingAtIndex: 0))
  expectEqual(
    "abc абв \u{0001F60A}XYZXY",
    "abc абв \u{0001F60A}".stringByPaddingToLength(
      15, withString: "XYZ", startingAtIndex: 0))
  expectEqual(
    "abc абв \u{0001F60A}YZXYZ",
    "abc абв \u{0001F60A}".stringByPaddingToLength(
      15, withString: "XYZ", startingAtIndex: 1))
}

NSStringAPIs.test("stringByRemovingPercentEncoding/OSX 10.9")
  .xfail(.OSXMinor(10, 9, reason: "looks like a bug in Foundation in OS X 10.9"))
  .xfail(.iOSMajor(7, reason: "same bug in Foundation in iOS 7.*"))
  .skip(.iOSSimulatorAny("same bug in Foundation in iOS Simulator 7.*"))
  .code {
  expectOptionalEqual("", "".stringByRemovingPercentEncoding)
}

NSStringAPIs.test("stringByRemovingPercentEncoding") {
  expectEmpty("%".stringByRemovingPercentEncoding)
  expectOptionalEqual(
    "abcd абвг",
    "ab%63d %D0%B0%D0%B1%D0%B2%D0%B3".stringByRemovingPercentEncoding)
}

NSStringAPIs.test("stringByReplacingCharactersInRange(_:withString:)") {
  do {
    let empty = ""
    expectEqual("", empty.stringByReplacingCharactersInRange(
      empty.startIndex..<empty.startIndex, withString: ""))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.stringByReplacingCharactersInRange(
    s.startIndex..<s.startIndex, withString: ""))
  expectEqual(s, s.stringByReplacingCharactersInRange(
    s.endIndex..<s.endIndex, withString: ""))
  expectEqual("zzz" + s, s.stringByReplacingCharactersInRange(
    s.startIndex..<s.startIndex, withString: "zzz"))
  expectEqual(s + "zzz", s.stringByReplacingCharactersInRange(
    s.endIndex..<s.endIndex, withString: "zzz"))

  expectEqual(
    "す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex..<s.startIndex.advancedBy(7), withString: ""))
  expectEqual(
    "zzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex..<s.startIndex.advancedBy(7), withString: "zzz"))
  expectEqual(
    "\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex..<s.startIndex.advancedBy(7), withString: "\u{1F602}"))

  expectEqual("\u{1F601}", s.stringByReplacingCharactersInRange(
    s.startIndex.successor()..<s.endIndex, withString: ""))
  expectEqual("\u{1F601}zzz", s.stringByReplacingCharactersInRange(
    s.startIndex.successor()..<s.endIndex, withString: "zzz"))
  expectEqual("\u{1F601}\u{1F602}", s.stringByReplacingCharactersInRange(
    s.startIndex.successor()..<s.endIndex, withString: "\u{1F602}"))

  expectEqual(
    "\u{1F601}aす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7), withString: ""))
  expectEqual(
    "\u{1F601}azzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7), withString: "zzz"))
  expectEqual(
    "\u{1F601}a\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingCharactersInRange(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7),
      withString: "\u{1F602}"))
}

NSStringAPIs.test("stringByReplacingOccurrencesOfString(_:withString:options:range:)") {
  do {
    let empty = ""
    expectEqual("", empty.stringByReplacingOccurrencesOfString(
      "", withString: ""))
    expectEqual("", empty.stringByReplacingOccurrencesOfString(
      "", withString: "xyz"))
    expectEqual("", empty.stringByReplacingOccurrencesOfString(
      "abc", withString: "xyz"))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.stringByReplacingOccurrencesOfString("", withString: "xyz"))
  expectEqual(s, s.stringByReplacingOccurrencesOfString("xyz", withString: ""))

  expectEqual("", s.stringByReplacingOccurrencesOfString(s, withString: ""))

  expectEqual(
    "\u{1F601}xyzbc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString("a", withString: "xyz"))

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "し\u{3099}", withString: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "し\u{3099}", withString: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "\u{3058}", withString: "xyz"))

  //
  // Use non-default 'options:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.LiteralSearch))

  expectEqual(s, s.stringByReplacingOccurrencesOfString(
    "\u{3058}", withString: "xyz",
    options: NSStringCompareOptions.LiteralSearch))

  //
  // Use non-default 'range:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.stringByReplacingOccurrencesOfString(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.LiteralSearch,
      range: s.startIndex..<s.startIndex.advancedBy(1)))

  expectEqual(s, s.stringByReplacingOccurrencesOfString(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.LiteralSearch,
      range: s.startIndex.advancedBy(1)..<s.startIndex.advancedBy(3)))
}

NSStringAPIs.test("stringByReplacingPercentEscapesUsingEncoding(_:)") {
  expectOptionalEqual(
    "abcd абвг",
    "abcd абвг".stringByReplacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))

  expectOptionalEqual(
    "abcd абвг\u{0000}\u{0001}",
    "abcd абвг%00%01".stringByReplacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))

  expectOptionalEqual(
    "abcd абвг",
    "%61%62%63%64%20%D0%B0%D0%B1%D0%B2%D0%B3"
      .stringByReplacingPercentEscapesUsingEncoding(NSUTF8StringEncoding))

  expectEmpty("%ED%B0".stringByReplacingPercentEscapesUsingEncoding(
    NSUTF8StringEncoding))

  expectEmpty("%zz".stringByReplacingPercentEscapesUsingEncoding(
    NSUTF8StringEncoding))
}

NSStringAPIs.test("stringByReplacingPercentEscapesUsingEncoding(_:)/rdar18029471")
  .xfail(
    .Custom({ true },
    reason: "<rdar://problem/18029471> NSString " +
      "stringByReplacingPercentEscapesUsingEncoding: does not return nil " +
      "when a byte sequence is not legal in ASCII"))
  .code {
  expectEmpty(
    "abcd%FF".stringByReplacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))
}

NSStringAPIs.test("stringByResolvingSymlinksInPath") {
  // <rdar://problem/18030188> Difference between
  // stringByResolvingSymlinksInPath and stringByStandardizingPath is unclear
  expectEqual("", "".stringByResolvingSymlinksInPath)
  expectEqual(
    "/var", "/private/var/tmp////..//".stringByResolvingSymlinksInPath)
}

NSStringAPIs.test("stringByStandardizingPath") {
  // <rdar://problem/18030188> Difference between
  // stringByResolvingSymlinksInPath and stringByStandardizingPath is unclear
  expectEqual("", "".stringByStandardizingPath)
  expectEqual(
    "/var", "/private/var/tmp////..//".stringByStandardizingPath)
}

NSStringAPIs.test("stringByTrimmingCharactersInSet(_:)") {
  expectEqual("", "".stringByTrimmingCharactersInSet(
    NSCharacterSet.decimalDigitCharacterSet()))

  expectEqual("abc", "abc".stringByTrimmingCharactersInSet(
    NSCharacterSet.decimalDigitCharacterSet()))

  expectEqual("", "123".stringByTrimmingCharactersInSet(
    NSCharacterSet.decimalDigitCharacterSet()))

  expectEqual("abc", "123abc789".stringByTrimmingCharactersInSet(
    NSCharacterSet.decimalDigitCharacterSet()))

  // Performs Unicode scalar comparison.
  expectEqual(
    "し\u{3099}abc",
    "し\u{3099}abc".stringByTrimmingCharactersInSet(
      NSCharacterSet(charactersInString: "\u{3058}")))
}

NSStringAPIs.test("stringsByAppendingPaths(_:)") {
  expectEqual([], "".stringsByAppendingPaths([]))
  expectEqual(
    [ "/tmp/foo", "/tmp/bar" ],
    "/tmp".stringsByAppendingPaths([ "foo", "bar" ]))
}

NSStringAPIs.test("substringFromIndex(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.substringFromIndex(s.startIndex))
  expectEqual("せ\u{3099}そ\u{3099}",
      s.substringFromIndex(s.startIndex.advancedBy(8)))
  expectEqual("", s.substringFromIndex(s.startIndex.advancedBy(10)))
}

NSStringAPIs.test("substringToIndex(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substringToIndex(s.startIndex))
  expectEqual("\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}",
      s.substringToIndex(s.startIndex.advancedBy(8)))
  expectEqual(s, s.substringToIndex(s.startIndex.advancedBy(10)))
}

NSStringAPIs.test("substringWithRange(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substringWithRange(s.startIndex..<s.startIndex))
  expectEqual(
    "",
    s.substringWithRange(s.startIndex.advancedBy(1)..<s.startIndex.advancedBy(1)))
  expectEqual("", s.substringWithRange(s.endIndex..<s.endIndex))
  expectEqual(s, s.substringWithRange(s.startIndex..<s.endIndex))
  expectEqual(
    "さ\u{3099}し\u{3099}す\u{3099}",
    s.substringWithRange(s.startIndex.advancedBy(5)..<s.startIndex.advancedBy(8)))
}

NSStringAPIs.test("localizedUppercaseString") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("ABCD", "abCD".localizedUppercaseString)
    }

    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("АБВГ", "абВГ".localizedUppercaseString)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("АБВГ", "абВГ".localizedUppercaseString)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedUppercaseString)
    }

    //
    // Special casing.
    //

    // U+0069 LATIN SMALL LETTER I
    // to upper case:
    // U+0049 LATIN CAPITAL LETTER I
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0049}", "\u{0069}".localizedUppercaseString)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0130}", "\u{0069}".localizedUppercaseString)
    }

    // U+00DF LATIN SMALL LETTER SHARP S
    // to upper case:
    // U+0053 LATIN CAPITAL LETTER S
    // U+0073 LATIN SMALL LETTER S
    // But because the whole string is converted to uppercase, we just get two
    // U+0053.
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0053}\u{0053}", "\u{00df}".localizedUppercaseString)
    }

    // U+FB01 LATIN SMALL LIGATURE FI
    // to upper case:
    // U+0046 LATIN CAPITAL LETTER F
    // U+0069 LATIN SMALL LETTER I
    // But because the whole string is converted to uppercase, we get U+0049
    // LATIN CAPITAL LETTER I.
    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("\u{0046}\u{0049}", "\u{fb01}".localizedUppercaseString)
    }
  }
}

NSStringAPIs.test("uppercaseStringWithLocale(_:)") {
  expectLocalizedEquality("ABCD", "abCD".uppercaseStringWithLocale, "en")

  expectLocalizedEquality("АБВГ", "абВГ".uppercaseStringWithLocale, "en")
  expectLocalizedEquality("АБВГ", "абВГ".uppercaseStringWithLocale, "ru")

  expectLocalizedEquality("たちつてと", "たちつてと".uppercaseStringWithLocale, "ru")

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality("\u{0049}", "\u{0069}".uppercaseStringWithLocale, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality("\u{0130}", "\u{0069}".uppercaseStringWithLocale, "tr")

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectLocalizedEquality("\u{0053}\u{0053}", "\u{00df}".uppercaseStringWithLocale, "en")

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectLocalizedEquality("\u{0046}\u{0049}", "\u{fb01}".uppercaseStringWithLocale, "ru")
}

NSStringAPIs.test("writeToFile(_:atomically:encoding:error:)") {
  let (_, nonExistentPath) = createNSStringTemporaryFile()
  do {
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    try s.writeToFile(
      nonExistentPath, atomically: false, encoding: NSASCIIStringEncoding)

    let content = try String(
      contentsOfFile: nonExistentPath, encoding: NSASCIIStringEncoding)

    expectEqual(s, content)
  } catch {
    expectUnreachableCatch(error)
  }
}

NSStringAPIs.test("writeToURL(_:atomically:encoding:error:)") {
  let (_, nonExistentPath) = createNSStringTemporaryFile()
  let nonExistentURL = NSURL(string: "file://" + nonExistentPath)!
  do {
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    try s.writeToURL(
      nonExistentURL, atomically: false, encoding: NSASCIIStringEncoding)

    let content = try String(
      contentsOfFile: nonExistentPath, encoding: NSASCIIStringEncoding)

    expectEqual(s, content)
  } catch {
    expectUnreachableCatch(error)
  }
}

NSStringAPIs.test("stringByApplyingTransform(_:reverse:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    do {
      let source = "tre\u{300}s k\u{fc}hl"
      expectEqual(
        "tres kuhl",
        source.stringByApplyingTransform(
          NSStringTransformStripDiacritics, reverse: false))
    }
    do {
      let source = "hiragana"
      expectEqual(
        "ひらがな",
        source.stringByApplyingTransform(
          NSStringTransformLatinToHiragana, reverse: false))
    }
    do {
      let source = "ひらがな"
      expectEqual(
        "hiragana",
        source.stringByApplyingTransform(
          NSStringTransformLatinToHiragana, reverse: true))
    }
  }
}

struct ComparisonTest {
  let expectedUnicodeCollation: ExpectedComparisonResult
  let lhs: String
  let rhs: String
  let loc: SourceLoc

  init(
    _ expectedUnicodeCollation: ExpectedComparisonResult,
    _ lhs: String, _ rhs: String,
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expectedUnicodeCollation = expectedUnicodeCollation
    self.lhs = lhs
    self.rhs = rhs
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

let comparisonTests = [
  ComparisonTest(.EQ, "", ""),
  ComparisonTest(.LT, "", "a"),

  // ASCII cases
  ComparisonTest(.LT, "t", "tt"),
  ComparisonTest(.GT, "t", "Tt"),
  ComparisonTest(.GT, "\u{0}", ""),
  ComparisonTest(.EQ, "\u{0}", "\u{0}"),
  // Currently fails:
  // ComparisonTest(.LT, "\r\n", "t"),
  // ComparisonTest(.GT, "\r\n", "\n"),
  // ComparisonTest(.LT, "\u{0}", "\u{0}\u{0}"),

  // Whitespace
  // U+000A LINE FEED (LF)
  // U+000B LINE TABULATION
  // U+000C FORM FEED (FF)
  // U+0085 NEXT LINE (NEL)
  // U+2028 LINE SEPARATOR
  // U+2029 PARAGRAPH SEPARATOR
  ComparisonTest(.GT, "\u{0085}", "\n"),
  ComparisonTest(.GT, "\u{000b}", "\n"),
  ComparisonTest(.GT, "\u{000c}", "\n"),
  ComparisonTest(.GT, "\u{2028}", "\n"),
  ComparisonTest(.GT, "\u{2029}", "\n"),
  ComparisonTest(.GT, "\r\n\r\n", "\r\n"),

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  ComparisonTest(.EQ, "a\u{301}", "\u{e1}"),
  ComparisonTest(.LT, "a", "a\u{301}"),
  ComparisonTest(.LT, "a", "\u{e1}"),

  // U+304B HIRAGANA LETTER KA
  // U+304C HIRAGANA LETTER GA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  ComparisonTest(.EQ, "\u{304b}", "\u{304b}"),
  ComparisonTest(.EQ, "\u{304c}", "\u{304c}"),
  ComparisonTest(.LT, "\u{304b}", "\u{304c}"),
  ComparisonTest(.LT, "\u{304b}", "\u{304c}\u{3099}"),
  ComparisonTest(.EQ, "\u{304c}", "\u{304b}\u{3099}"),
  ComparisonTest(.LT, "\u{304c}", "\u{304c}\u{3099}"),

  // U+212B ANGSTROM SIGN
  // U+030A COMBINING RING ABOVE
  // U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
  ComparisonTest(.EQ, "\u{212b}", "A\u{30a}"),
  ComparisonTest(.EQ, "\u{212b}", "\u{c5}"),
  ComparisonTest(.EQ, "A\u{30a}", "\u{c5}"),
  ComparisonTest(.LT, "A\u{30a}", "a"),
  ComparisonTest(.LT, "A", "A\u{30a}"),

  // U+2126 OHM SIGN
  // U+03A9 GREEK CAPITAL LETTER OMEGA
  ComparisonTest(.EQ, "\u{2126}", "\u{03a9}"),

  // U+0323 COMBINING DOT BELOW
  // U+0307 COMBINING DOT ABOVE
  // U+1E63 LATIN SMALL LETTER S WITH DOT BELOW
  // U+1E69 LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
  ComparisonTest(.EQ, "\u{1e69}", "s\u{323}\u{307}"),
  ComparisonTest(.EQ, "\u{1e69}", "s\u{307}\u{323}"),
  ComparisonTest(.EQ, "\u{1e69}", "\u{1e63}\u{307}"),
  ComparisonTest(.EQ, "\u{1e63}", "s\u{323}"),
  ComparisonTest(.EQ, "\u{1e63}\u{307}", "s\u{323}\u{307}"),
  ComparisonTest(.EQ, "\u{1e63}\u{307}", "s\u{307}\u{323}"),
  ComparisonTest(.LT, "s\u{323}", "\u{1e69}"),

  // U+FB01 LATIN SMALL LIGATURE FI
  ComparisonTest(.EQ, "\u{fb01}", "\u{fb01}"),
  ComparisonTest(.LT, "fi", "\u{fb01}"),

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
  ComparisonTest(.EQ, "\u{0301}", "\u{0341}"),
  ComparisonTest(.LT, "\u{0301}", "\u{0954}"),
  ComparisonTest(.LT, "\u{0341}", "\u{0954}"),
]

func checkStringComparison(
  expected: ExpectedComparisonResult,
  _ lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
  // String / String
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expected.isEQ(), lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

  expectEqual(expected.isLT(), lhs < rhs, stackTrace: stackTrace)
  expectEqual(expected.isLE(), lhs <= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGE(), lhs >= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGT(), lhs > rhs, stackTrace: stackTrace)
  checkComparable(expected, lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

  // NSString / NSString
  let lhsNSString = lhs as NSString
  let rhsNSString = rhs as NSString
  let expectedEqualUnicodeScalars =
    Array(lhs.unicodeScalars) == Array(rhs.unicodeScalars)
  // FIXME: Swift String and NSString comparison may not be equal.
  expectEqual(
    expectedEqualUnicodeScalars, lhsNSString == rhsNSString,
    stackTrace: stackTrace)
  expectEqual(
    !expectedEqualUnicodeScalars, lhsNSString != rhsNSString,
    stackTrace: stackTrace)
  checkHashable(
    expectedEqualUnicodeScalars, lhsNSString, rhsNSString,
    stackTrace: stackTrace.withCurrentLoc())
}

NSStringAPIs.test("String.{Equatable,Hashable,Comparable}") {
  for test in comparisonTests {
    checkStringComparison(
      test.expectedUnicodeCollation, test.lhs, test.rhs,
      test.loc.withCurrentLoc())
    checkStringComparison(
      test.expectedUnicodeCollation.flip(), test.rhs, test.lhs,
      test.loc.withCurrentLoc())
  }
}

func checkCharacterComparison(
  expected: ExpectedComparisonResult,
  _ lhs: Character, _ rhs: Character, _ stackTrace: SourceLocStack
) {
  // Character / Character
  expectEqual(expected.isEQ(), lhs == rhs, stackTrace: stackTrace)
  expectEqual(expected.isNE(), lhs != rhs, stackTrace: stackTrace)
  checkHashable(
    expected.isEQ(), lhs, rhs, stackTrace: stackTrace.withCurrentLoc())

  expectEqual(expected.isLT(), lhs < rhs, stackTrace: stackTrace)
  expectEqual(expected.isLE(), lhs <= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGE(), lhs >= rhs, stackTrace: stackTrace)
  expectEqual(expected.isGT(), lhs > rhs, stackTrace: stackTrace)
  checkComparable(expected, lhs, rhs, stackTrace: stackTrace.withCurrentLoc())
}

NSStringAPIs.test("Character.{Equatable,Hashable,Comparable}") {
  for test in comparisonTests {
    if test.lhs.characters.count == 1 && test.rhs.characters.count == 1 {
      let lhsCharacter = Character(test.lhs)
      let rhsCharacter = Character(test.rhs)
      checkCharacterComparison(
        test.expectedUnicodeCollation, lhsCharacter, rhsCharacter,
        test.loc.withCurrentLoc())
      checkCharacterComparison(
        test.expectedUnicodeCollation.flip(), rhsCharacter, lhsCharacter,
        test.loc.withCurrentLoc())
    }
  }
}

func checkHasPrefixHasSuffix(
  lhs: String, _ rhs: String, _ stackTrace: SourceLocStack
) {
  if lhs == "" {
    return
  }
  if rhs == "" {
    expectFalse(lhs.hasPrefix(rhs), stackTrace: stackTrace)
    expectFalse(lhs.hasSuffix(rhs), stackTrace: stackTrace)
    return
  }

  // To determine the expected results, compare grapheme clusters,
  // scalar-to-scalar, of the NFD form of the strings.
  let lhsNFDGraphemeClusters =
    lhs.decomposedStringWithCanonicalMapping.characters.map {
      Array(String($0).unicodeScalars)
    }
  let rhsNFDGraphemeClusters =
    rhs.decomposedStringWithCanonicalMapping.characters.map {
      Array(String($0).unicodeScalars)
    }
  let expectHasPrefix = lhsNFDGraphemeClusters.startsWith(
    rhsNFDGraphemeClusters, isEquivalent: (==))
  let expectHasSuffix =
    lhsNFDGraphemeClusters.lazy.reverse().startsWith(
      rhsNFDGraphemeClusters.lazy.reverse(), isEquivalent: (==))

  expectEqual(expectHasPrefix, lhs.hasPrefix(rhs), stackTrace: stackTrace)
  expectEqual(
    expectHasPrefix, (lhs + "abc").hasPrefix(rhs), stackTrace: stackTrace)
  expectEqual(expectHasSuffix, lhs.hasSuffix(rhs), stackTrace: stackTrace)
  expectEqual(
    expectHasSuffix, ("abc" + lhs).hasSuffix(rhs), stackTrace: stackTrace)
}

NSStringAPIs.test("hasPrefix,hasSuffix") {
  for test in comparisonTests {
    checkHasPrefixHasSuffix(test.lhs, test.rhs, test.loc.withCurrentLoc())
    checkHasPrefixHasSuffix(test.rhs, test.lhs, test.loc.withCurrentLoc())
  }
}

NSStringAPIs.test("Failures{hasPrefix,hasSuffix}-CF")
  .xfail(.Custom({ true }, reason: "rdar://problem/19034601")).code {
  let test = ComparisonTest(.LT, "\u{0}", "\u{0}\u{0}")
  checkHasPrefixHasSuffix(test.lhs, test.rhs, test.loc.withCurrentLoc())
}

NSStringAPIs.test("Failures{hasPrefix,hasSuffix}")
  .xfail(.Custom({ true }, reason: "blocked on rdar://problem/19036555")).code {
  let tests =
    [ComparisonTest(.LT, "\r\n", "t"), ComparisonTest(.GT, "\r\n", "\n")]
  tests.forEach {
    checkHasPrefixHasSuffix($0.lhs, $0.rhs, $0.loc.withCurrentLoc())
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
  expectFalse(xs == ys)
  expectTrue(xs != ys)
  expectTrue(ys == ys)
  expectFalse(ys != ys)
}

NSStringAPIs.test("CompareStringsWithUnpairedSurrogates")
  .xfail(
    .Custom({ true },
    reason: "<rdar://problem/18029104> Strings referring to underlying " +
      "storage with unpaired surrogates compare unequal"))
  .code {
  let donor = "abcdef"
  let acceptor = "\u{1f601}\u{1f602}\u{1f603}"

  expectEqual("\u{fffd}\u{1f602}\u{fffd}",
    acceptor[donor.startIndex.advancedBy(1)..<donor.startIndex.advancedBy(5)])
}

NSStringAPIs.test("copy construction") {
  let expected = "abcd"
  let x = NSString(string: expected as NSString)
  expectEqual(expected, x as String)
  let y = NSMutableString(string: expected as NSString)
  expectEqual(expected, y as String)
}

var CStringTests = TestSuite("CStringTests")

func getNullCString() -> UnsafeMutablePointer<CChar> {
  return nil
}

func getASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<CChar>.alloc(100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (up, { up.dealloc(100) })
}

func getNonASCIICString() -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String1(
) -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x80
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.dealloc(100) })
}

func getIllFormedUTF8String2(
) -> (UnsafeMutablePointer<CChar>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.alloc(100)
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
  do {
    let s = getNullCString()
    expectEmpty(String.fromCString(s))
  }
  do {
    let (s, dealloc) = getASCIICString()
    expectOptionalEqual("ab", String.fromCString(s))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIICString()
    expectOptionalEqual("аб", String.fromCString(s))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    expectEmpty(String.fromCString(s))
    dealloc()
  }
}

CStringTests.test("String.fromCStringRepairingIllFormedUTF8") {
  do {
    let s = getNullCString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectEmpty(result)
    expectFalse(hadError)
  }
  do {
    let (s, dealloc) = getASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("ab", result)
    expectFalse(hadError)
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIICString()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("аб", result)
    expectFalse(hadError)
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    let (result, hadError) = String.fromCStringRepairingIllFormedUTF8(s)
    expectOptionalEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
    expectTrue(hadError)
    dealloc()
  }
}

runAllTests()

