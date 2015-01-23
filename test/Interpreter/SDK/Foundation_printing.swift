// RUN: %target-run-simple-swift
// XFAIL: interpret

import Foundation
import StdlibUnittest

var FoundationPrinting = TestSuite("FoundationPrinting")

FoundationPrinting.test("OverlayTypesHaveDescription") {
  func hasDescription(_: Printable) {}

  var a: ObjCBool = true
  hasDescription(a)
}

FoundationPrinting.test("ObjCBoolPrinting") {
  var true_: ObjCBool = true
  var false_: ObjCBool = false
  expectPrinted("true", true_)
  expectPrinted("false", false_)
}

FoundationPrinting.test("SelectorPrinting") {
  expectPrinted("", Selector(""))
  expectPrinted(":", Selector(":"))
  expectPrinted("a", Selector("a"))
  expectPrinted("abc", Selector("abc"))
  expectPrinted("abc:", Selector("abc:"))
  expectPrinted("abc:def:", Selector("abc:def:"))
}

// Pure Swift classes with and without a custom description
class PureSwiftDescription {
  @objc var description: String = "awesome"
}
class PureSwiftNoDescription {
  var x = 318
}

FoundationPrinting.test("NSArrayDescription") {
  var array = NSArray(objects:PureSwiftDescription(), PureSwiftNoDescription())
  var expected = 
    "(\n" +
    "    awesome,\n" +
    "    \"main.PureSwiftNoDescription\"\n" +
    ")"
  expectPrinted(expected, array)
}

runAllTests()

