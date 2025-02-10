// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var WithoutEscapingSuite = TestSuite("WithoutActuallyEscaping")

var sink: Any = ()

func dontEscape(f: () -> ()) {
  withoutActuallyEscaping(f) {
    $0()
  }
}

func letEscape(f: () -> ()) -> () -> () {
  return withoutActuallyEscaping(f) { return $0 }
}

var testShouldThrow = false

struct MyError : Error {}

@inline(never)
func letEscapeThrowing(f: () -> ()) throws -> () -> () {
  return try withoutActuallyEscaping(f) {
    if testShouldThrow {
      throw MyError()
    }
    return $0
  }
}

@inline(never)
func letEscapeOnThrowingPath(f: () throws -> ()) throws {
  try withoutActuallyEscaping(f) {
    sink = $0
    throw MyError()
  }
}


WithoutEscapingSuite.test("ExpectNoCrash") {
  dontEscape(f: { print("foo") })
}

WithoutEscapingSuite.test("ExpectDebugCrash") {
  // Optimize versions pass a nil closure context.
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  sink = letEscape(f: { print("foo") })
}

struct Context {
  var a = 0
  var b = 1
}

WithoutEscapingSuite.test("ExpectCrash") {
  expectCrashLater()
  let context = Context()
  sink = letEscape(f: { print("Context: \(context.a) \(context.b)") })
}

WithoutEscapingSuite.test("ExpectNonThrowingCrash") {
  expectCrashLater()
  let context = Context()
  var testDidThrow = false
  testShouldThrow = false
  do {
    sink = try letEscapeThrowing(f: { print("Context: \(context.a) \(context.b)") })
  } catch {
    testDidThrow = true
  }
  expectFalse(testDidThrow)
}

WithoutEscapingSuite.test("ExpectThrowingNoCrash") {
  let context = Context()
  var testDidThrow = false
  testShouldThrow = true
  do {
    sink = try letEscapeThrowing(f: { print("Context: \(context.a) \(context.b)") })
  } catch {
    testDidThrow = true
  }
  expectTrue(testDidThrow)
}

WithoutEscapingSuite.test("ExpectThrowingCrash") {
  expectCrashLater()
  let context = Context()
  var testDidThrow = false
  do {
    try letEscapeOnThrowingPath(f: { print("Context: \(context.a) \(context.b)") })
  } catch {
    testDidThrow = true
  }
  expectTrue(testDidThrow)
}

runAllTests()
