// RUN: %sourcekitd-test -req=index %s -- -serialize-diagnostics-path %t.dia %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

// This test verifies that, when Objective-C interop is disabled, private
// methods are *not* surfaced as "test candidates".
// FIXME: Ideally this test would run on any OS, provided Objective-C interop
//        was disabled.
// REQUIRES: OS=linux-gnu

func test_takesNoParams_andReturnsVoid_butIsNotAnInstanceMethod() {}

struct MyStruct {
  func test_startsWithTest_takesNoParams_returnsVoid_butIsDefinedOnAStruct() {}
}
class XCTestCase {}
private class MyPrivateClass : XCTestCase {
  func test_startsWithTest_takesNoParams_returnsVoid_butIsPrivate() {}
}

public class MyClass : XCTestCase {
  func doesNotStartWithTest() {}
  func test_startsWithTest_butTakesAParam(param: Int) {}
  func test_startsWithTest_andTakesNoParams_butReturnsNonVoid() -> Int {}
  private func test_startsWithTest_takesNoParams_andReturnsVoid_butIsPrivate() {}
  func test_startsWithTest_takesNoParams_returnsVoid() {}
  func test_startsWithTest_takesNoParams_returnsVoid_andThrows() throws {}
}

