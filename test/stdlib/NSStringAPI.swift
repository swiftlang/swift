// RUN: %target-run-simple-swift %S/Inputs/NSStringAPI_test.txt | FileCheck %s

//
// Tests for the NSString APIs as exposed by String
//

import Foundation

var _anyExpectFailed = false

func expectEqual<T : Equatable>(
    expected: T, actual: T,
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

func expectEqual<T : Equatable>(
    expected: T, actual: T?,
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

func expectNotEqual<T : Equatable>(
    expected: T, actual: T,
    file: String = __FILE__, line: UWord = __LINE__
) {
  if expected == actual {
    _anyExpectFailed = true
    println("check failed at \(file), line \(line)")
    println("unexpected value: \"\(actual)\"")
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
      println("All tests passed")
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
  expectEqual(true, contains(availableEncodings, defaultCStringEncoding))

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
  // FIXME
}

NSStringAPIs.test("capitalizedString") {
  // FIXME
}

NSStringAPIs.test("capitalizedStringWithLocale") {
  // FIXME
}

NSStringAPIs.test("caseInsensitiveCompare(_:)") {
  // FIXME
}

NSStringAPIs.test("commonPrefixWithString(_:options:)") {
  // FIXME
}

NSStringAPIs.test("compare(_:options:range:locale:)") {
  // FIXME
}

NSStringAPIs.test("completePathIntoString(_:caseSensitive:matchesIntoArray:filterTypes)") {

  // FIXME
  if true {
    var outputName = "None Found"
    var count = nonExistentPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(0, count)
    expectEqual("None Found", outputName)
  }

  if true {
    var outputName = "None Found"
    var count = existingPath.completePathIntoString(
        &outputName, caseSensitive: false)

    expectEqual(1, count)
    expectEqual(existingPath, outputName)
  }

}

NSStringAPIs.test("componentsSeparatedByCharactersInSet(_:)") {
  // FIXME
}

NSStringAPIs.test("componentsSeparatedByString(_:)") {
  // FIXME
}

NSStringAPIs.test("cStringUsingEncoding(_:)") {
  // FIXME
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
// CHECK: All tests passed

