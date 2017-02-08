// RUN: %sourcekitd-test -req=index %s -- -serialize-diagnostics-path %t.dia %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

// This test verifies that, when Objective-C interop is enabled, all "test
// candidate" methods are surfaced regardless of visibility. (On Linux, only
// internal or public methods are considered "test candidates".)
// REQUIRES: objc_interop

func test_takesNoParams_andReturnsVoid_butIsNotAnInstanceMethod() {}

struct MyStruct {
  func test_startsWithTest_takesNoParams_returnsVoid_butIsDefinedOnAStruct() {}
}
class XCTestCase {}
private class MyPrivateClass : XCTestCase {
  func test_startsWithTest_takesNoParams_returnsVoid_andIsPrivate() {}
}

public class MyClass : XCTestCase {
  func doesNotStartWithTest() {}
  func test_startsWithTest_butTakesAParam(param: Int) {}
  func test_startsWithTest_andTakesNoParams_butReturnsNonVoid() -> Int {}
  private func test_startsWithTest_takesNoParams_returnsVoid_andIsPrivate() {}
  func test_startsWithTest_takesNoParams_returnsVoid() {}
  func test_startsWithTest_takesNoParams_returnsVoid_andThrows() throws {}
}

