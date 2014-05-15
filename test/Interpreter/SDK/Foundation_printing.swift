// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func foundationOverlayTypesHaveDescription() {
  func hasDescription(_: Printable) {}

  var a: ObjCBool = true
  hasDescription(a)
}

func printedIs<T>(
    object: T, expected: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  var actual = toString(object)
  if expected != actual {
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    assert(expected == actual, file: file, line: line)
  }
}

func test_ObjCBoolPrinting() {
  var true_: ObjCBool = true
  var false_: ObjCBool = false
  printedIs(true_, "true")
  printedIs(false_, "false")

  println("test_ObjCBoolPrinting done")
}
test_ObjCBoolPrinting()
// CHECK: test_ObjCBoolPrinting done

func test_SelectorPrinting() {
  printedIs(Selector(""), "")
  printedIs(Selector(":"), ":")
  printedIs(Selector("a"), "a")
  printedIs(Selector("abc"), "abc")
  printedIs(Selector("abc:"), "abc:")
  printedIs(Selector("abc:def:"), "abc:def:")

  println("test_SelectorPrinting done")
}
test_SelectorPrinting()
// CHECK: test_SelectorPrinting done

