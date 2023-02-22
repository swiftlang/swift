// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

func myGlobalFunction() -> Invalid {}

struct Test {
  func myInstanceMethod() -> Invalid {}

  func testInstanceMethod() {
    Test.myInstanceMethod#^INSTANCE_METHOD^#
    // Check that we don't crash
    // INSTANCE_METHOD-NOT: Begin completions
  }

  func testGlobalFunctionMethod() {
    myGlobalFunction#^GLOBAL_FUNCTION^#
    // Check that we don't crash
    // GLOBAL_FUNCTION: Begin completions
    // GLOBAL_FUNCTION: Keyword[self]/CurrNominal:          .self[#_#]
    // GLOBAL_FUNCTION: End completions
  }

  func testLocalFunction() {
    func myLocalFunction() -> Invalid {}
    myLocalFunction#^LOCAL_FUNCTION^#
    // LOCAL_FUNCTION: Begin completions
    // LOCAL_FUNCTION: Keyword[self]/CurrNominal:          .self[#_#]
    // LOCAL_FUNCTION: End completions
  }
}

