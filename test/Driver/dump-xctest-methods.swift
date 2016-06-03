// RUN: %target-swift-frontend -dump-xctest-methods %s 2>&1 | FileCheck %s

// CHECK-NOT: test_takesNoParams_andReturnsVoid_butIsNotAnInstanceMethod
func test_takesNoParams_andReturnsVoid_butIsNotAnInstanceMethod() {}

class MyClass {
  // CHECK-NOT: doesNotStartWithTest
  func doesNotStartWithTest() {}

  // CHECK-NOT: test_takesAParam
  func test_startsWithTest_butTakesAParam(param: Int) {}

  // CHECK-NOT: test_startsWithTest_andTakesNoParams_butReturnsNonVoid
  func test_startsWithTest_andTakesNoParams_butReturnsNonVoid() -> Int {}

  // CHECK: s:FC4main7MyClass45test_startsWithTest_takesNoParams_returnsVoidFT_T_
  func test_startsWithTest_takesNoParams_returnsVoid() {}

  // CHECK: s:FC4main7MyClass55test_startsWithTest_takesNoParams_returnsVoid_andThrowsFzT_T_
  func test_startsWithTest_takesNoParams_returnsVoid_andThrows() throws {}
}

