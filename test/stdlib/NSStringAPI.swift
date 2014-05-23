// RUN: rm -rf %t && mkdir -p %t && %S/../../utils/gyb %s -o %t/NSStringAPI.swift
// RUN: %S/../../utils/line-directive %t/NSStringAPI.swift -- %target-build-swift -module-cache-path %t/clang-module-cache %t/NSStringAPI.swift -o %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/NSStringAPI_test.txt > /tmp/z.txt
// RU/N: %target-run %t/a.out %S/Inputs/NSStringAPI_test.txt | %S/../../utils/line-directive %t/NSStringAPI.swift -- FileCheck %s

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

%for EquatableType in ['String', 'Int', 'UInt', 'NSComparisonResult']:

func expectEqual(
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

func expectEqual(
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

func expectNotEqual(
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
  // FIXME: crashes because of a null pointer
  //expectEmpty("абв".cStringUsingEncoding(NSASCIIStringEncoding))

  // FIXME: crashes because of a non-ASCII string
  //let expectedBytes: UInt8[] = [ 0xd0, 0xb0, 0xd0, 0xb1, 0xd0, 0xb2 ]
  //var expectedStr: CChar[] = expectedBytes.map { $0.asSigned() }
  //expectEqual(expectedStr,
  //    "абв".cStringUsingEncoding(NSUTF8StringEncoding))
}

NSStringAPIs.test("dataUsingEncoding(_:allowLossyConversion:)") {
  // FIXME
}

NSStringAPIs.test("decomposedStringWithCanonicalMapping") {
  // FIXME
}

NSStringAPIs.test("decomposedStringWithCompatibilityMapping") {
  // FIXME
}

NSStringAPIs.test("enumerateLines(_:)") {
  // FIXME
}

NSStringAPIs.test("enumerateLinguisticTagsInRange(_:scheme:options:orthography:_:") {
  // FIXME
}

NSStringAPIs.test("enumerateSubstringsInRange(_:options:_:)") {
  // FIXME
}

NSStringAPIs.test("fastestEncoding") {
  // FIXME
}

NSStringAPIs.test("fileSystemRepresentation()") {
  // FIXME
}

NSStringAPIs.test("getBytes(_:maxLength:usedLength:encoding:options:range:remainingRange:)") {
  // FIXME
}

NSStringAPIs.test("getCString(_:maxLength:encoding:)") {
  // FIXME
}

NSStringAPIs.test("getFileSystemRepresentation(_:maxLength:)") {
  // FIXME
}

NSStringAPIs.test("getLineStart(_:end:contentsEnd:forRange:)") {
  // FIXME
}

NSStringAPIs.test("getParagraphStart(_:end:contentsEnd:forRange:)") {
  // FIXME
}

NSStringAPIs.test("hash") {
  // FIXME
}

NSStringAPIs.test("stringWithBytes(_:length:encoding:)") {
  // FIXME
}

NSStringAPIs.test("stringWithBytesNoCopy(_:length:encoding:freeWhenDone:)") {
  // FIXME
}

NSStringAPIs.test("init(utf16CodeUnits:count:)") {
  var chars: unichar[] = [
    unichar("s".value), unichar("o".value), unichar("x".value) ]

  var sox = String(utf16CodeUnits: chars, count: chars.count)
  expectEqual("sox", sox)
}

NSStringAPIs.test("init(utf16CodeUnitsNoCopy:count:freeWhenDone:)") {
  // FIXME
}

NSStringAPIs.test("init(format:_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld", world, 42))
}

NSStringAPIs.test("init(format:arguments:)") {
  // FIXME
}

NSStringAPIs.test("init(format:locale:_:...)") {
  var world: NSString = "world"
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: nil, world, 42))
  expectEqual("Hello, world!%42", String(format: "Hello, %@!%%%ld",
      locale: NSLocale.systemLocale(), world, 42))
}

NSStringAPIs.test("init(format:locale:arguments:)") {
  // FIXME
}

NSStringAPIs.test("lastPathComponent") {
  // FIXME
}

NSStringAPIs.test("utf16count") {
  // FIXME
}

NSStringAPIs.test("lengthOfBytesUsingEncoding(_:)") {
  // FIXME
}

NSStringAPIs.test("lineRangeForRange(_:)") {
  // FIXME
}

NSStringAPIs.test("linguisticTagsInRange(_:scheme:options:orthography:tokenRanges:)") {
  // FIXME
}

NSStringAPIs.test("localizedCaseInsensitiveCompare(_:)") {
  // FIXME
}

NSStringAPIs.test("localizedCompare(_:)") {
  // FIXME
}

NSStringAPIs.test("localizedStandardCompare(_:)") {
  // FIXME
}

NSStringAPIs.test("lowercaseStringWithLocale(_:)") {
  // FIXME
}

NSStringAPIs.test("maximumLengthOfBytesUsingEncoding(_:)") {
  // FIXME
}

NSStringAPIs.test("paragraphRangeForRange(_:)") {
  // FIXME
}

NSStringAPIs.test("pathComponents") {
  // FIXME
}

NSStringAPIs.test("pathExtension") {
  // FIXME
}

NSStringAPIs.test("precomposedStringWithCanonicalMapping") {
  // FIXME
}

NSStringAPIs.test("precomposedStringWithCompatibilityMapping") {
  // FIXME
}

NSStringAPIs.test("propertyList()") {
  // FIXME
}

NSStringAPIs.test("propertyListFromStringsFileFormat()") {
  // FIXME
}

NSStringAPIs.test("rangeOfCharacterFromSet(_:options:range:)") {
  // FIXME
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
    /*
    FIXME: currently 'debugDescription' depends on the locale.
    var (s, dealloc) = getNonASCIICString()
    expectEqual("\"аб\"", s.debugDescription)
    dealloc()
    */
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
