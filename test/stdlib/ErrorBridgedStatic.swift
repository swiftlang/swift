// RUN: %empty-directory(%t)
// RUN: %target-clang -fmodules -c -o %t/ErrorBridgedStaticImpl.o %S/Inputs/ErrorBridgedStaticImpl.m 
// RUN: %target-build-swift -static-stdlib -o %t/ErrorBridgedStatic %t/ErrorBridgedStaticImpl.o %s -import-objc-header %S/Inputs/ErrorBridgedStaticImpl.h
// RUN: strip %t/ErrorBridgedStatic
// RUN: %target-run %t/ErrorBridgedStatic

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: static_stdlib

import StdlibUnittest

class Bar: Foo {
  override func foo(_ x: Int32) throws {
    try super.foo(5)
  }
}

var ErrorBridgingStaticTests = TestSuite("ErrorBridging with static libs")

ErrorBridgingStaticTests.test("round-trip Swift override of ObjC method") {
  do {
    try (Bar() as Foo).foo(5)
  } catch { }
}

runAllTests()
