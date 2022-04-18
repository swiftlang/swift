// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: optimized_stdlib

import StdlibUnittest
import StdlibUnicodeUnittest
import Foundation

let StringGraphemeBreaking = TestSuite("StringGraphemeBreaking")

extension String {
  var backwardsCount: Int {
    var c = 0
    var index = endIndex
    while index != startIndex {
      c += 1
      formIndex(before: &index)
    }
    return c
  }
}

if #available(SwiftStdlib 5.6, *) {
  StringGraphemeBreaking.test("grapheme breaking") {
    for graphemeBreakTest in graphemeBreakTests {
      expectEqual(
        graphemeBreakTest.1,
        graphemeBreakTest.0.count,
        "string: \(String(reflecting: graphemeBreakTest.0))")
      expectEqual(
        graphemeBreakTest.1,
        graphemeBreakTest.0.backwardsCount,
        "string: \(String(reflecting: graphemeBreakTest.0))")
    }
  }
}

// The most simple subclass of NSString that CoreFoundation does not know
// about.
class NonContiguousNSString : NSString {
  required init(coder aDecoder: NSCoder) {
    fatalError("don't call this initializer")
  }
  required init(itemProviderData data: Data, typeIdentifier: String) throws {
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

extension _StringGuts {
  @_silgen_name("$ss11_StringGutsV9isForeignSbvg")
  func _isForeign() -> Bool
}

func getUTF16Array(from string: String) -> [UInt16] {
  var result: [UInt16] = []

  for cp in string.utf16 {
    result.append(cp)
  }

  return result
}

if #available(SwiftStdlib 5.6, *) {
  StringGraphemeBreaking.test("grapheme breaking foreign") {
    for graphemeBreakTest in graphemeBreakTests {
      let foreignTest = NonContiguousNSString(
        getUTF16Array(from: graphemeBreakTest.0)
      )
      let test = foreignTest as String

      expectTrue(test._guts._isForeign())
      expectEqual(
        graphemeBreakTest.1, test.count,
        "string: \(String(reflecting: graphemeBreakTest.0))")
      expectEqual(
        graphemeBreakTest.1, test.backwardsCount,
        "string: \(String(reflecting: graphemeBreakTest.0))")
    }
  }
}

runAllTests()
