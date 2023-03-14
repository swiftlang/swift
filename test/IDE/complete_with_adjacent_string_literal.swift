// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

func takeClosure(x: () -> Void) {}
func takeString(_ a: String) -> MyStruct {}

struct MyStruct {
  func style() {}
}

func foo() {
  takeClosure {
    takeString("\(1)")
      .style(#^COMPLETE^#)
  }
}
