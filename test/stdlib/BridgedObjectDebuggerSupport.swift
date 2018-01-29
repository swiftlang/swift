// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Foundation
import StdlibUnittest

let StringForPrintObjectTests = TestSuite("StringForPrintObject")

// Wrap stringForPrintObject for convenience. Note that the debugger uses
// something slightly different to pretty-print (see: debugVal()).
func printObj<T>(_ x: T) -> String {
  return _DebuggerSupport.stringForPrintObject(x)
}

// Check if @x has a reference type.
func hasReferenceType<T>(_ x: T) -> Bool {
  return _canBeClass(T.self) == 1
}

// The debugger uses unsafeBitCast to take an arbitrary address and cast it to
// AnyObject. Mimic that operation here.
func debugVal<T>(_ x: inout T) -> String {
  if !hasReferenceType(x) {
    return printObj(x)
  }
  return withUnsafePointer(to: &x) {
    return _DebuggerSupport.stringForPrintObject(Swift.unsafeBitCast($0.pointee, to: AnyObject.self))
  }
}

StringForPrintObjectTests.test("Basic") {
  var a = "Hello World" as NSString
  let a_printed = printObj(a)
  let a_debug = debugVal(&a)
  expectEqual("Hello World", String(reflecting: a))
  expectEqual("Hello World\n", a_printed)
  expectEqual(a_printed, a_debug)
}

StringForPrintObjectTests.test("NSStringFromStringLiteral") {
  var a = Foundation.NSString(stringLiteral: "Hello World")
  let a_printed = printObj(a)
  let a_debug = debugVal(&a)
  expectEqual("Hello World", String(reflecting: a))
  expectEqual("Hello World\n", a_printed)
  expectEqual(a_printed, a_debug)
}

StringForPrintObjectTests.test("NSStringFromUnsafeBuffer") {
  let buf = UnsafeMutablePointer<Int8>.allocate(capacity: 8)
  buf[0] = 65
  buf[1] = 0
  var a = Foundation.NSString(utf8String: buf)!
  let a_printed = printObj(a)
  let a_debug = debugVal(&a)
  expectEqual("A", String(reflecting: a))
  expectEqual("A\n", a_printed)
  expectEqual(a_printed, a_debug)
  buf.deallocate()
}

StringForPrintObjectTests.test("ArrayOfStrings") {
  var a = ["Hello World" as NSString]
  let a_printed = printObj(a)
  let a_debug = debugVal(&a)
  expectEqual("[Hello World]", String(reflecting: a))
  expectEqual("▿ 1 element\n  - 0 : Hello World\n", a_printed)
  expectEqual(a_printed, a_debug)
}

struct StructWithOneMember {
  var a = "Hello World" as NSString
}

StringForPrintObjectTests.test("StructWithOneMember") {
  var a = StructWithOneMember()
  let a_printed = printObj(StructWithOneMember())
  let a_debug = debugVal(&a)
  expectEqual("main.StructWithOneMember(a: Hello World)", String(reflecting: a))
  expectEqual("▿ StructWithOneMember\n  - a : Hello World\n", a_printed)
  expectEqual(a_printed, a_debug)
}

struct StructWithTwoMembers {
  var a = 1
  var b = "Hello World" as NSString
}

StringForPrintObjectTests.test("StructWithTwoMembers") {
  var a = StructWithTwoMembers()
  let a_printed = printObj(StructWithTwoMembers())
  let a_debug = debugVal(&a)
  expectEqual("main.StructWithTwoMembers(a: 1, b: Hello World)", String(reflecting: a))
  expectEqual("▿ StructWithTwoMembers\n  - a : 1\n  - b : Hello World\n", a_printed)
  expectEqual(a_printed, a_debug)
}

runAllTests()
