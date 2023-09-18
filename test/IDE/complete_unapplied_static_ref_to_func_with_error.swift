// RUN: %batch-code-completion
// Check that we don't crash

func myGlobalFunction() -> Invalid {}

struct Test {
  func myInstanceMethod() -> Invalid {}

  func testInstanceMethod() {
    Test.myInstanceMethod#^INSTANCE_METHOD?check=NO_RESULTS^#
  }

  func testGlobalFunctionMethod() {
    myGlobalFunction#^GLOBAL_FUNCTION?check=NO_RESULTS^#
    // Check that we don't crash
  }

  func testLocalFunction() {
    func myLocalFunction() -> Invalid {}
    myLocalFunction#^LOCAL_FUNCTION?check=NO_RESULTS^#
  }
}

// NO_RESULTS-NOT: Begin completions
