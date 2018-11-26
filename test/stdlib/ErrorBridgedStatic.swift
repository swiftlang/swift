// RUN: %empty-directory(%t)
// RUN: %target-clang -fmodules -c -o %t/ErrorBridgedStaticImpl.o %S/Inputs/ErrorBridgedStaticImpl.m 
// RUN: %target-build-swift -static-stdlib -o %t/ErrorBridgedStatic %t/ErrorBridgedStaticImpl.o %s -import-objc-header %S/Inputs/ErrorBridgedStaticImpl.h
// RUN: strip %t/ErrorBridgedStatic
// RUN: %target-run %t/ErrorBridgedStatic

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: static_stdlib
// REQUIRES: rdar42789939

import StdlibUnittest

class Bar: Foo {
  override func foo(_ x: Int32) throws {
    try super.foo(5)
  }
  
  override func foothrows(_ x: Int32) throws {
    try super.foothrows(5)
  }
}

var ErrorBridgingStaticTests = TestSuite("ErrorBridging with static libs")

ErrorBridgingStaticTests.test("round-trip Swift override of ObjC method") {
  do {
    try (Bar() as Foo).foo(5)
  } catch { }
}

ErrorBridgingStaticTests.test("round-trip Swift override of throwing ObjC method") {
  do {
    try (Bar() as Foo).foothrows(5)
  } catch {
    print(error)
    expectEqual(error._domain, "abcd")
    expectEqual(error._code, 1234)
  }
}

runAllTests()
