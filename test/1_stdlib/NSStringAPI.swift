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

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

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

  @objc(copyWithZone:) override func copy(withZone zone: NSZone) -> AnyObject {
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


NSStringAPIs.test("init(contentsOf:encoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = NSURL(string: "file://" + existingPath)!
  let nonExistentURL = NSURL(string: "file://" + nonExistentPath)!
  do {
    let content = try String(
      contentsOf: existingURL, encoding: NSASCIIStringEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  do {
    _ = try String(contentsOf: nonExistentURL, encoding: NSASCIIStringEncoding)
    expectUnreachable()
  } catch {
  }
}

NSStringAPIs.test("init(contentsOf:usedEncoding:error:)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  let existingURL = NSURL(string: "file://" + existingPath)!
  let nonExistentURL = NSURL(string: "file://" + nonExistentPath)!
  do {
    var usedEncoding: NSStringEncoding = 0
    let content = try String(
      contentsOf: existingURL, usedEncoding: &usedEncoding)

    expectNotEqual(0, usedEncoding)
    expectEqual(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit,",
      content._lines[0])
  } catch {
    expectUnreachableCatch(error)
  }

  var usedEncoding: NSStringEncoding = 0
  do {
    _ = try String(contentsOf: nonExistentURL, usedEncoding: &usedEncoding)
    expectUnreachable()
  } catch {
    expectEqual(0, usedEncoding)
  }
}

NSStringAPIs.test("init(cString_:encoding:)") {
  expectOptionalEqual("foo, a basmati bar!",
      String(cString: 
          "foo, a basmati bar!", encoding: String.defaultCStringEncoding()))
}

NSStringAPIs.test("init(utf8String:)") {
  var s = "foo あいう"
  var up = UnsafeMutablePointer<UInt8>(allocatingCapacity: 100)
  var i = 0
  for b in s.utf8 {
    up[i] = b
    i += 1
  }
  up[i] = 0
  expectOptionalEqual(s, String(utf8String: UnsafePointer(up)))
  up.deallocateCapacity(100)
}

NSStringAPIs.test("canBeConvertedToEncoding(_:)") {
  expectTrue("foo".canBeConvertedToEncoding(NSASCIIStringEncoding))
  expectFalse("あいう".canBeConvertedToEncoding(NSASCIIStringEncoding))
}

NSStringAPIs.test("capitalized") {
  expectEqual("Foo Foo Foo Foo", "foo Foo fOO FOO".capitalized)
  expectEqual("Жжж", "жжж".capitalized)
}

NSStringAPIs.test("localizedCapitalized") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
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
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("Iii Iii", "iii III".localizedCapitalized)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("tr") {
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
  expected: String,
  _ op: (_: NSLocale?) -> String,
  _ localeID: String? = nil,
  @autoclosure _ message: () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),  
  file: String = #file, line: UInt = #line
) {
  let trace = stackTrace.pushIf(showFrame, file: file, line: line)

  let locale = localeID.map {
    NSLocale(localeIdentifier: $0)
  } ?? NSLocale.current()
  
  expectEqual(
    expected, op(locale),
    message(), stackTrace: trace)
  
  expectEqual(
    op(NSLocale.system()), op(nil),
    message(), stackTrace: trace)
}

NSStringAPIs.test("capitalizedStringWith(_:)") {
  expectLocalizedEquality(
    "Foo Foo Foo Foo",
    "foo Foo fOO FOO".capitalizedStringWith)
  
  expectLocalizedEquality("Жжж","жжж".capitalizedStringWith)

  expectEqual(
    "Foo Foo Foo Foo",
    "foo Foo fOO FOO".capitalizedStringWith(nil))
  expectEqual("Жжж", "жжж".capitalizedStringWith(nil))

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality(
    "Iii Iii",
    "iii III".capitalizedStringWith, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality(
    "İii Iıı",
    "iii III".capitalizedStringWith, "tr")
}

NSStringAPIs.test("caseInsensitiveCompare(_:)") {
  expectEqual(NSComparisonResult.orderedSame,
      "abCD".caseInsensitiveCompare("AbCd"))
  expectEqual(NSComparisonResult.orderedAscending,
      "abCD".caseInsensitiveCompare("AbCdE"))

  expectEqual(NSComparisonResult.orderedSame,
      "абвг".caseInsensitiveCompare("АбВг"))
  expectEqual(NSComparisonResult.orderedAscending,
      "абВГ".caseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("commonPrefixWith(_:options:)") {
  expectEqual("ab",
      "abcd".commonPrefixWith("abdc", options: []))
  expectEqual("abC",
      "abCd".commonPrefixWith("abce", options: .caseInsensitiveSearch))

  expectEqual("аб",
      "абвг".commonPrefixWith("абгв", options: []))
  expectEqual("абВ",
      "абВг".commonPrefixWith("абвд", options: .caseInsensitiveSearch))
}

NSStringAPIs.test("compare(_:options:range:locale:)") {
  expectEqual(NSComparisonResult.orderedSame,
      "abc".compare("abc"))
  expectEqual(NSComparisonResult.orderedAscending,
      "абв".compare("где"))

  expectEqual(NSComparisonResult.orderedSame,
      "abc".compare("abC", options: .caseInsensitiveSearch))
  expectEqual(NSComparisonResult.orderedSame,
      "абв".compare("абВ", options: .caseInsensitiveSearch))

  do {
    let s = "abcd"
    let r = s.startIndex.successor()..<s.endIndex
    expectEqual(NSComparisonResult.orderedSame,
        s.compare("bcd", range: r))
  }
  do {
    let s = "абвг"
    let r = s.startIndex.successor()..<s.endIndex
    expectEqual(NSComparisonResult.orderedSame,
        s.compare("бвг", range: r))
  }

  expectEqual(NSComparisonResult.orderedSame,
      "abc".compare("abc", locale: NSLocale.current()))
  expectEqual(NSComparisonResult.orderedSame,
      "абв".compare("абв", locale: NSLocale.current()))
}

NSStringAPIs.test("completePathInto(_:caseSensitive:matchesInto:filterTypes)") {
  let (existingPath, nonExistentPath) = createNSStringTemporaryFile()
  do {
    var count = nonExistentPath.completePathInto(caseSensitive: false)
    expectEqual(0, count)
  }

  do {
    var outputName = "None Found"
    var count = nonExistentPath.completePathInto(
        &outputName, caseSensitive: false)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = [ "foo", "bar" ]
    var count = nonExistentPath.completePathInto(
        &outputName, caseSensitive: false, matchesInto: &outputArray)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
    expectEqual([ "foo", "bar" ], outputArray)
  }

  do {
    var count = existingPath.completePathInto(caseSensitive: false)
    expectEqual(1, count)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePathInto(
        &outputName, caseSensitive: false)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }

  do {
    var outputName = "None Found"
    var outputArray: [String] = [ "foo", "bar" ]
    var count = existingPath.completePathInto(
        &outputName, caseSensitive: false, matchesInto: &outputArray)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
    expectEqual([ existingPath ], outputArray)
  }

  do {
    var outputName = "None Found"
    var count = existingPath.completePathInto(
        &outputName, caseSensitive: false, filterTypes: [ "txt" ])

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }
}

NSStringAPIs.test("componentsSeparatedByCharactersIn(_:)") {
  expectEqual([ "" ], "".componentsSeparatedByCharactersIn(
    NSCharacterSet.decimalDigit()))

  expectEqual(
    [ "абв", "", "あいう", "abc" ],
    "абв12あいう3abc".componentsSeparatedByCharactersIn(
        NSCharacterSet.decimalDigit()))

  expectEqual(
    [ "абв", "", "あいう", "abc" ],
    "абв\u{1F601}\u{1F602}あいう\u{1F603}abc"
      .componentsSeparatedByCharactersIn(
        NSCharacterSet(charactersIn: "\u{1F601}\u{1F602}\u{1F603}")))

  // Performs Unicode scalar comparison.
  expectEqual(
    [ "abcし\u{3099}def" ],
    "abcし\u{3099}def".componentsSeparatedByCharactersIn(
      NSCharacterSet(charactersIn: "\u{3058}")))
}

NSStringAPIs.test("componentsSeparatedBy(_:)") {
  expectEqual([ "" ], "".componentsSeparatedBy("//"))

  expectEqual(
    [ "абв", "あいう", "abc" ],
    "абв//あいう//abc".componentsSeparatedBy("//"))

  // Performs normalization.
  expectEqual(
    [ "abc", "def" ],
    "abcし\u{3099}def".componentsSeparatedBy("\u{3058}"))
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

NSStringAPIs.test("enumerateLinguisticTagsIn(_:scheme:options:orthography:_:") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = s.startIndex.advancedBy(5)
  let endIndex = s.startIndex.advancedBy(62)
  var tags: [String] = []
  var tokens: [String] = []
  var sentences: [String] = []
  s.enumerateLinguisticTagsIn(startIndex..<endIndex,
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

NSStringAPIs.test("enumerateSubstringsIn(_:options:_:)") {
  let s = "え\u{304b}\u{3099}お\u{263a}\u{fe0f}😀😊"
  let startIndex = s.startIndex.advancedBy(1)
  let endIndex = s.startIndex.advancedBy(5)
  do {
    var substrings: [String] = []
    s.enumerateSubstringsIn(startIndex..<endIndex,
      options: NSStringEnumerationOptions.byComposedCharacterSequences) {
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
    s.enumerateSubstringsIn(startIndex..<endIndex,
      options: [.byComposedCharacterSequences, .substringNotRequired]) {
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
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
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
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
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
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
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
    var buffer = [UInt8](repeating: 0xff, count: bufferLength)
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
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
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
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = s.getCString(&buffer, maxLength: 100,
      encoding: NSUTF8StringEncoding)
    expectTrue(result)
    expectEqualSequence(expectedStr, buffer)
  }
  do {
    // Limit buffer size with 'maxLength'.
    let bufferLength = 100
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
    let result = s.getCString(&buffer, maxLength: 8,
      encoding: NSUTF8StringEncoding)
    expectFalse(result)
  }
  do {
    // String with unpaired surrogates.
    let illFormedUTF16 = NonContiguousNSString([ 0xd800 ]) as String
    let bufferLength = 100
    var buffer = Array(
      repeating: CChar(bitPattern: 0xff), count: bufferLength)
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
  let args: [CVarArg] = [ world, 42 ]
  expectEqual("Hello, world!%42",
      String(format: "Hello, %@!%%%ld", arguments: args))
}

NSStringAPIs.test("init(format:locale:_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, world, 42))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.system(), world, 42))
}

NSStringAPIs.test("init(format:locale:arguments:)") {
  let world: NSString = "world"
  let args: [CVarArg] = [ world, 42 ]
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, arguments: args))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.system(), arguments: args))
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

NSStringAPIs.test("lineRangeFor(_:)") {
  let s = "Глокая куздра\nштеко будланула\nбокра и кудрячит\nбокрёнка."
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
    let result = s.lineRangeFor(r)
    expectEqual("штеко будланула\nбокра и кудрячит\n", s[result])
  }
}

NSStringAPIs.test("linguisticTagsIn(_:scheme:options:orthography:tokenRanges:)") {
  let s = "Абв. Глокая куздра штеко будланула бокра и кудрячит бокрёнка. Абв."
  let startIndex = s.startIndex.advancedBy(5)
  let endIndex = s.startIndex.advancedBy(17)
  var tokenRanges: [Range<String.Index>] = []
  var tags = s.linguisticTagsIn(startIndex..<endIndex,
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
  expectEqual(NSComparisonResult.orderedSame,
      "abCD".localizedCaseInsensitiveCompare("AbCd"))
  expectEqual(NSComparisonResult.orderedAscending,
      "abCD".localizedCaseInsensitiveCompare("AbCdE"))

  expectEqual(NSComparisonResult.orderedSame,
      "абвг".localizedCaseInsensitiveCompare("АбВг"))
  expectEqual(NSComparisonResult.orderedAscending,
      "абВГ".localizedCaseInsensitiveCompare("АбВгД"))
}

NSStringAPIs.test("localizedCompare(_:)") {
  expectEqual(NSComparisonResult.orderedAscending,
      "abCD".localizedCompare("AbCd"))

  expectEqual(NSComparisonResult.orderedAscending,
      "абвг".localizedCompare("АбВг"))
}

NSStringAPIs.test("localizedStandardCompare(_:)") {
  expectEqual(NSComparisonResult.orderedAscending,
      "abCD".localizedStandardCompare("AbCd"))

  expectEqual(NSComparisonResult.orderedAscending,
      "абвг".localizedStandardCompare("АбВг"))
}

NSStringAPIs.test("localizedLowercase") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("abcd", "abCD".localizedLowercase)
    }

    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("абвг", "абВГ".localizedLowercase)
    }
    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("абвг", "абВГ".localizedLowercase)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedLowercase)
    }

    //
    // Special casing.
    //

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0069}\u{0307}", "\u{0130}".localizedLowercase)
    }

    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0130}".localizedLowercase)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case:
    // U+0069 LATIN SMALL LETTER I
    // U+0307 COMBINING DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual(
        "\u{0069}\u{0307}",
        "\u{0049}\u{0307}".localizedLowercase)
    }

    // U+0049 LATIN CAPITAL LETTER I
    // U+0307 COMBINING DOT ABOVE
    // to lower case in Turkish locale:
    // U+0069 LATIN SMALL LETTER I
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0069}", "\u{0049}\u{0307}".localizedLowercase)
    }
  }
}

NSStringAPIs.test("lowercaseStringWith(_:)") {
  expectLocalizedEquality("abcd", "abCD".lowercaseStringWith, "en")

  expectLocalizedEquality("абвг", "абВГ".lowercaseStringWith, "en")
  expectLocalizedEquality("абвг", "абВГ".lowercaseStringWith, "ru")

  expectLocalizedEquality("たちつてと", "たちつてと".lowercaseStringWith, "ru")

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", "\u{0130}".lowercaseStringWith, "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", "\u{0130}".lowercaseStringWith, "tr")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercaseStringWith, "en")

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality("\u{0069}", "\u{0049}\u{0307}".lowercaseStringWith, "tr")
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

NSStringAPIs.test("paragraphRangeFor(_:)") {
  let s = "Глокая куздра\nштеко будланула\u{2028}бокра и кудрячит\u{2028}бокрёнка.\n Абв."
  let r = s.startIndex.advancedBy(16)..<s.startIndex.advancedBy(35)
  do {
    let result = s.paragraphRangeFor(r)
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

NSStringAPIs.test("rangeOfCharacterFrom(_:options:range:)") {
  do {
    let charset = NSCharacterSet(charactersIn: "абв")
    do {
      let s = "Глокая куздра"
      let r = s.rangeOfCharacterFrom(charset)!
      expectEqual(s.startIndex.advancedBy(4), r.startIndex)
      expectEqual(s.startIndex.advancedBy(5), r.endIndex)
    }
    do {
      expectEmpty("клмн".rangeOfCharacterFrom(charset))
    }
    do {
      let s = "абвклмнабвклмн"
      let r = s.rangeOfCharacterFrom(charset,
          options: .backwardsSearch)!
      expectEqual(s.startIndex.advancedBy(9), r.startIndex)
      expectEqual(s.startIndex.advancedBy(10), r.endIndex)
    }
    do {
      let s = "абвклмнабв"
      let r = s.rangeOfCharacterFrom(charset,
          range: s.startIndex.advancedBy(3)..<s.endIndex)!
      expectEqual(s.startIndex.advancedBy(7), r.startIndex)
      expectEqual(s.startIndex.advancedBy(8), r.endIndex)
    }
  }

  do {
    let charset = NSCharacterSet(charactersIn: "\u{305f}\u{3099}")
    expectEmpty("\u{3060}".rangeOfCharacterFrom(charset))
  }
  do {
    let charset = NSCharacterSet(charactersIn: "\u{3060}")
    expectEmpty("\u{305f}\u{3099}".rangeOfCharacterFrom(charset))
  }

  do {
    let charset = NSCharacterSet(charactersIn: "\u{1F600}")
    do {
      let s = "abc\u{1F600}"
      expectEqual("\u{1F600}",
          s[s.rangeOfCharacterFrom(charset)!])
    }
    do {
      expectEmpty("abc\u{1F601}".rangeOfCharacterFrom(charset))
    }
  }
}

NSStringAPIs.test("rangeOfComposedCharacterSequenceAt(_:)") {
  let s = "\u{1F601}abc \u{305f}\u{3099} def"
  expectEqual("\u{1F601}", s[s.rangeOfComposedCharacterSequenceAt(
      s.startIndex)])
  expectEqual("a", s[s.rangeOfComposedCharacterSequenceAt(
      s.startIndex.advancedBy(1))])
  expectEqual("\u{305f}\u{3099}", s[s.rangeOfComposedCharacterSequenceAt(
      s.startIndex.advancedBy(5))])
  expectEqual(" ", s[s.rangeOfComposedCharacterSequenceAt(
      s.startIndex.advancedBy(6))])
}

NSStringAPIs.test("rangeOfComposedCharacterSequencesFor(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("\u{1F601}a", s[s.rangeOfComposedCharacterSequencesFor(
      s.startIndex..<s.startIndex.advancedBy(2))])
  expectEqual("せ\u{3099}そ\u{3099}", s[s.rangeOfComposedCharacterSequencesFor(
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

NSStringAPIs.test("rangeOf(_:options:range:locale:)") {
  do {
    let s = ""
    expectEmpty(s.rangeOf(""))
    expectEmpty(s.rangeOf("abc"))
  }
  do {
    let s = "abc"
    expectEmpty(s.rangeOf(""))
    expectEmpty(s.rangeOf("def"))
    expectOptionalEqual(0..<3, toIntRange(s, s.rangeOf("abc")))
  }
  do {
    let s = "さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"
    expectOptionalEqual(2..<3, toIntRange(s, s.rangeOf("す\u{3099}")))
    expectOptionalEqual(2..<3, toIntRange(s, s.rangeOf("\u{305a}")))

    expectEmpty(s.rangeOf("\u{3099}す"))
    expectEmpty(s.rangeOf("す"))

    // Note: here `rangeOf` API produces indexes that don't point between
    // grapheme cluster boundaries -- these cannot be created with public
    // String interface.
    //
    // FIXME: why does this search succeed and the above queries fail?  There is
    // no apparent pattern.
    expectEqual("\u{3099}", s[s.rangeOf("\u{3099}")!])
  }
  do {
    let s = "а\u{0301}б\u{0301}в\u{0301}г\u{0301}"
    expectOptionalEqual(0..<1, toIntRange(s, s.rangeOf("а\u{0301}")))
    expectOptionalEqual(1..<2, toIntRange(s, s.rangeOf("б\u{0301}")))

    expectEmpty(s.rangeOf("б"))
    expectEmpty(s.rangeOf("\u{0301}б"))

    // FIXME: Again, indexes that don't correspond to grapheme
    // cluster boundaries.
    expectEqual("\u{0301}", s[s.rangeOf("\u{0301}")!])
  }
}

NSStringAPIs.test("contains(_:)") {
  withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
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

  withOverriddenNSLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".contains("ı"))
  }
}

NSStringAPIs.test("localizedCaseInsensitiveContains(_:)") {
  withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
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

  withOverriddenNSLocaleCurrentLocale("tr") {
    expectFalse("\u{0130}".localizedCaseInsensitiveContains("ı"))
  }
}

NSStringAPIs.test("localizedStandardContains(_:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
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

    withOverriddenNSLocaleCurrentLocale("tr") {
      expectTrue("\u{0130}".localizedStandardContains("ı"))
    }
  }
}

NSStringAPIs.test("localizedStandardRangeOf(_:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    func rangeOf(string: String, _ substring: String) -> Range<Int>? {
      return toIntRange(
        string, string.localizedStandardRangeOf(substring))
    }
    withOverriddenNSLocaleCurrentLocale("en") { () -> Void in
      expectEmpty(rangeOf("", ""))
      expectEmpty(rangeOf("", "a"))
      expectEmpty(rangeOf("a", ""))
      expectEmpty(rangeOf("a", "b"))
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
          "\u{0301}", s[s.localizedStandardRangeOf("\u{0301}")!])
      }
      expectEmpty(rangeOf("a", "\u{0301}"))

      expectEqual(0..<1, rangeOf("i", "I"))
      expectEqual(0..<1, rangeOf("I", "i"))
      expectEqual(0..<1, rangeOf("\u{0130}", "i"))
      expectEqual(0..<1, rangeOf("i", "\u{0130}"))
      return ()
    }

    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual(0..<1, rangeOf("\u{0130}", "ı"))
    }
  }
}

NSStringAPIs.test("smallestEncoding") {
  let availableEncodings: [NSStringEncoding] = String.availableStringEncodings()
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

NSStringAPIs.test("addingPercentEscapesUsingEncoding(_:)") {
  expectEmpty(
    "abcd абвг".addingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))
  expectOptionalEqual("abcd%20%D0%B0%D0%B1%D0%B2%D0%B3",
    "abcd абвг".addingPercentEscapesUsingEncoding(
      NSUTF8StringEncoding))
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

NSStringAPIs.test("appendingPathComponent(_:)") {
  expectEqual("", "".appendingPathComponent(""))
  expectEqual("a.txt", "".appendingPathComponent("a.txt"))
  expectEqual("/tmp/a.txt", "/tmp".appendingPathComponent("a.txt"))
}

NSStringAPIs.test("appending(_:)") {
  expectEqual("", "".appending(""))
  expectEqual("a", "a".appending(""))
  expectEqual("a", "".appending("a"))
  expectEqual("さ\u{3099}", "さ".appending("\u{3099}"))
}

NSStringAPIs.test("deletingLastPathComponent") {
  expectEqual("", "".deletingLastPathComponent)
  expectEqual("/", "/".deletingLastPathComponent)
  expectEqual("/", "/tmp".deletingLastPathComponent)
  expectEqual("/tmp", "/tmp/a.txt".deletingLastPathComponent)
}

NSStringAPIs.test("folding(options:locale:)") {

  func fwo(
    s: String, _ options: NSStringCompareOptions
  ) -> (NSLocale?) -> String {
    return { loc in s.folding(options: options, locale: loc) }
  }
  
  expectLocalizedEquality("abcd", fwo("abCD", .caseInsensitiveSearch), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectLocalizedEquality(
    "\u{0069}\u{0307}", fwo("\u{0130}", .caseInsensitiveSearch), "en")

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case in Turkish locale:
  // U+0069 LATIN SMALL LETTER I
  expectLocalizedEquality(
    "\u{0069}", fwo("\u{0130}", .caseInsensitiveSearch), "tr")

  expectLocalizedEquality(
    "example123", fwo("ｅｘａｍｐｌｅ１２３", .widthInsensitiveSearch), "en")
}

NSStringAPIs.test("byPaddingToLength(_:withString:startingAtIndex:)") {
  expectEqual(
    "abc абв \u{0001F60A}",
    "abc абв \u{0001F60A}".byPaddingToLength(
      10, withString: "XYZ", startingAt: 0))
  expectEqual(
    "abc абв \u{0001F60A}XYZXY",
    "abc абв \u{0001F60A}".byPaddingToLength(
      15, withString: "XYZ", startingAt: 0))
  expectEqual(
    "abc абв \u{0001F60A}YZXYZ",
    "abc абв \u{0001F60A}".byPaddingToLength(
      15, withString: "XYZ", startingAt: 1))
}

NSStringAPIs.test("removingPercentEncoding/OSX 10.9")
  .xfail(.OSXMinor(10, 9, reason: "looks like a bug in Foundation in OS X 10.9"))
  .xfail(.iOSMajor(7, reason: "same bug in Foundation in iOS 7.*"))
  .skip(.iOSSimulatorAny("same bug in Foundation in iOS Simulator 7.*"))
  .code {
  expectOptionalEqual("", "".removingPercentEncoding)
}

NSStringAPIs.test("removingPercentEncoding") {
  expectEmpty("%".removingPercentEncoding)
  expectOptionalEqual(
    "abcd абвг",
    "ab%63d %D0%B0%D0%B1%D0%B2%D0%B3".removingPercentEncoding)
}

NSStringAPIs.test("replacingCharactersIn(_:withString:)") {
  do {
    let empty = ""
    expectEqual("", empty.replacingCharactersIn(
      empty.startIndex..<empty.startIndex, withString: ""))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.replacingCharactersIn(
    s.startIndex..<s.startIndex, withString: ""))
  expectEqual(s, s.replacingCharactersIn(
    s.endIndex..<s.endIndex, withString: ""))
  expectEqual("zzz" + s, s.replacingCharactersIn(
    s.startIndex..<s.startIndex, withString: "zzz"))
  expectEqual(s + "zzz", s.replacingCharactersIn(
    s.endIndex..<s.endIndex, withString: "zzz"))

  expectEqual(
    "す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex..<s.startIndex.advancedBy(7), withString: ""))
  expectEqual(
    "zzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex..<s.startIndex.advancedBy(7), withString: "zzz"))
  expectEqual(
    "\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex..<s.startIndex.advancedBy(7), withString: "\u{1F602}"))

  expectEqual("\u{1F601}", s.replacingCharactersIn(
    s.startIndex.successor()..<s.endIndex, withString: ""))
  expectEqual("\u{1F601}zzz", s.replacingCharactersIn(
    s.startIndex.successor()..<s.endIndex, withString: "zzz"))
  expectEqual("\u{1F601}\u{1F602}", s.replacingCharactersIn(
    s.startIndex.successor()..<s.endIndex, withString: "\u{1F602}"))

  expectEqual(
    "\u{1F601}aす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7), withString: ""))
  expectEqual(
    "\u{1F601}azzzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7), withString: "zzz"))
  expectEqual(
    "\u{1F601}a\u{1F602}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingCharactersIn(
      s.startIndex.advancedBy(2)..<s.startIndex.advancedBy(7),
      withString: "\u{1F602}"))
}

NSStringAPIs.test("replacingOccurrencesOf(_:withString:options:range:)") {
  do {
    let empty = ""
    expectEqual("", empty.replacingOccurrencesOf(
      "", withString: ""))
    expectEqual("", empty.replacingOccurrencesOf(
      "", withString: "xyz"))
    expectEqual("", empty.replacingOccurrencesOf(
      "abc", withString: "xyz"))
  }

  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.replacingOccurrencesOf("", withString: "xyz"))
  expectEqual(s, s.replacingOccurrencesOf("xyz", withString: ""))

  expectEqual("", s.replacingOccurrencesOf(s, withString: ""))

  expectEqual(
    "\u{1F601}xyzbc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf("a", withString: "xyz"))

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "し\u{3099}", withString: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "し\u{3099}", withString: "xyz"))

  expectEqual(
    "\u{1F601}abc さ\u{3099}xyzす\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "\u{3058}", withString: "xyz"))

  //
  // Use non-default 'options:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.literalSearch))

  expectEqual(s, s.replacingOccurrencesOf(
    "\u{3058}", withString: "xyz",
    options: NSStringCompareOptions.literalSearch))

  //
  // Use non-default 'range:'
  //

  expectEqual(
    "\u{1F602}\u{1F603}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}",
    s.replacingOccurrencesOf(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.literalSearch,
      range: s.startIndex..<s.startIndex.advancedBy(1)))

  expectEqual(s, s.replacingOccurrencesOf(
      "\u{1F601}", withString: "\u{1F602}\u{1F603}",
      options: NSStringCompareOptions.literalSearch,
      range: s.startIndex.advancedBy(1)..<s.startIndex.advancedBy(3)))
}

NSStringAPIs.test("replacingPercentEscapesUsingEncoding(_:)") {
  expectOptionalEqual(
    "abcd абвг",
    "abcd абвг".replacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))

  expectOptionalEqual(
    "abcd абвг\u{0000}\u{0001}",
    "abcd абвг%00%01".replacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))

  expectOptionalEqual(
    "abcd абвг",
    "%61%62%63%64%20%D0%B0%D0%B1%D0%B2%D0%B3"
      .replacingPercentEscapesUsingEncoding(NSUTF8StringEncoding))

  expectEmpty("%ED%B0".replacingPercentEscapesUsingEncoding(
    NSUTF8StringEncoding))

  expectEmpty("%zz".replacingPercentEscapesUsingEncoding(
    NSUTF8StringEncoding))
}

NSStringAPIs.test("replacingPercentEscapesUsingEncoding(_:)/rdar18029471")
  .xfail(
    .Custom({ true },
    reason: "<rdar://problem/18029471> NSString " +
      "replacingPercentEscapesUsingEncoding: does not return nil " +
      "when a byte sequence is not legal in ASCII"))
  .code {
  expectEmpty(
    "abcd%FF".replacingPercentEscapesUsingEncoding(
      NSASCIIStringEncoding))
}

NSStringAPIs.test("resolvingSymlinksInPath") {
  // <rdar://problem/18030188> Difference between
  // resolvingSymlinksInPath and stringByStandardizingPath is unclear
  expectEqual("", "".resolvingSymlinksInPath)
  expectEqual(
    "/var", "/private/var/tmp////..//".resolvingSymlinksInPath)
}

NSStringAPIs.test("standardizingPath") {
  // <rdar://problem/18030188> Difference between
  // resolvingSymlinksInPath and standardizingPath is unclear
  expectEqual("", "".standardizingPath)
  expectEqual(
    "/var", "/private/var/tmp////..//".standardizingPath)
}

NSStringAPIs.test("byTrimmingCharactersIn(_:)") {
  expectEqual("", "".byTrimmingCharactersIn(
    NSCharacterSet.decimalDigit()))

  expectEqual("abc", "abc".byTrimmingCharactersIn(
    NSCharacterSet.decimalDigit()))

  expectEqual("", "123".byTrimmingCharactersIn(
    NSCharacterSet.decimalDigit()))

  expectEqual("abc", "123abc789".byTrimmingCharactersIn(
    NSCharacterSet.decimalDigit()))

  // Performs Unicode scalar comparison.
  expectEqual(
    "し\u{3099}abc",
    "し\u{3099}abc".byTrimmingCharactersIn(
      NSCharacterSet(charactersIn: "\u{3058}")))
}

NSStringAPIs.test("stringsByAppendingPaths(_:)") {
  expectEqual([], "".stringsByAppendingPaths([]))
  expectEqual(
    [ "/tmp/foo", "/tmp/bar" ],
    "/tmp".stringsByAppendingPaths([ "foo", "bar" ]))
}

NSStringAPIs.test("substringFrom(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual(s, s.substringFrom(s.startIndex))
  expectEqual("せ\u{3099}そ\u{3099}",
      s.substringFrom(s.startIndex.advancedBy(8)))
  expectEqual("", s.substringFrom(s.startIndex.advancedBy(10)))
}

NSStringAPIs.test("substringTo(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substringTo(s.startIndex))
  expectEqual("\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}",
      s.substringTo(s.startIndex.advancedBy(8)))
  expectEqual(s, s.substringTo(s.startIndex.advancedBy(10)))
}

NSStringAPIs.test("substringWith(_:)") {
  let s = "\u{1F601}abc さ\u{3099}し\u{3099}す\u{3099}せ\u{3099}そ\u{3099}"

  expectEqual("", s.substringWith(s.startIndex..<s.startIndex))
  expectEqual(
    "",
    s.substringWith(s.startIndex.advancedBy(1)..<s.startIndex.advancedBy(1)))
  expectEqual("", s.substringWith(s.endIndex..<s.endIndex))
  expectEqual(s, s.substringWith(s.startIndex..<s.endIndex))
  expectEqual(
    "さ\u{3099}し\u{3099}す\u{3099}",
    s.substringWith(s.startIndex.advancedBy(5)..<s.startIndex.advancedBy(8)))
}

NSStringAPIs.test("localizedUppercase") {
  if #available(OSX 10.11, iOS 9.0, *) {
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("ABCD", "abCD".localizedUppercase)
    }

    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("АБВГ", "абВГ".localizedUppercase)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("АБВГ", "абВГ".localizedUppercase)
    }

    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("たちつてと", "たちつてと".localizedUppercase)
    }

    //
    // Special casing.
    //

    // U+0069 LATIN SMALL LETTER I
    // to upper case:
    // U+0049 LATIN CAPITAL LETTER I
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0049}", "\u{0069}".localizedUppercase)
    }

    // U+0069 LATIN SMALL LETTER I
    // to upper case in Turkish locale:
    // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
    withOverriddenNSLocaleCurrentLocale("tr") {
      expectEqual("\u{0130}", "\u{0069}".localizedUppercase)
    }

    // U+00DF LATIN SMALL LETTER SHARP S
    // to upper case:
    // U+0053 LATIN CAPITAL LETTER S
    // U+0073 LATIN SMALL LETTER S
    // But because the whole string is converted to uppercase, we just get two
    // U+0053.
    withOverriddenNSLocaleCurrentLocale("en") {
      expectEqual("\u{0053}\u{0053}", "\u{00df}".localizedUppercase)
    }

    // U+FB01 LATIN SMALL LIGATURE FI
    // to upper case:
    // U+0046 LATIN CAPITAL LETTER F
    // U+0069 LATIN SMALL LETTER I
    // But because the whole string is converted to uppercase, we get U+0049
    // LATIN CAPITAL LETTER I.
    withOverriddenNSLocaleCurrentLocale("ru") {
      expectEqual("\u{0046}\u{0049}", "\u{fb01}".localizedUppercase)
    }
  }
}

NSStringAPIs.test("uppercaseStringWith(_:)") {
  expectLocalizedEquality("ABCD", "abCD".uppercaseStringWith, "en")

  expectLocalizedEquality("АБВГ", "абВГ".uppercaseStringWith, "en")
  expectLocalizedEquality("АБВГ", "абВГ".uppercaseStringWith, "ru")

  expectLocalizedEquality("たちつてと", "たちつてと".uppercaseStringWith, "ru")

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectLocalizedEquality("\u{0049}", "\u{0069}".uppercaseStringWith, "en")

  // U+0069 LATIN SMALL LETTER I
  // to upper case in Turkish locale:
  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  expectLocalizedEquality("\u{0130}", "\u{0069}".uppercaseStringWith, "tr")

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectLocalizedEquality("\u{0053}\u{0053}", "\u{00df}".uppercaseStringWith, "en")

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectLocalizedEquality("\u{0046}\u{0049}", "\u{fb01}".uppercaseStringWith, "ru")
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

NSStringAPIs.test("applyingTransform(_:reverse:)") {
  if #available(OSX 10.11, iOS 9.0, *) {
    do {
      let source = "tre\u{300}s k\u{fc}hl"
      expectEqual(
        "tres kuhl",
        source.applyingTransform(
          NSStringTransformStripDiacritics, reverse: false))
    }
    do {
      let source = "hiragana"
      expectEqual(
        "ひらがな",
        source.applyingTransform(
          NSStringTransformLatinToHiragana, reverse: false))
    }
    do {
      let source = "ひらがな"
      expectEqual(
        "hiragana",
        source.applyingTransform(
          NSStringTransformLatinToHiragana, reverse: true))
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

runAllTests()

