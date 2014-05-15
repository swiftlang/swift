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

