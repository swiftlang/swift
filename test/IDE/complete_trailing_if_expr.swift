// RUN: %batch-code-completion

struct MyStruct {
  func takeAnotherClosure(_ y: () -> Void) {}
}

func takeClosure(_ x: () -> Void) -> MyStruct {}

func foo() {
  takeClosure {
    #^COMPLETE^#
  }.takeAnotherClosure {
    if true {
      1
    } else {
      1
    }
  }
}

// COMPLETE: Begin completions
// COMPLETE: Decl[Struct]/CurrModule:            MyStruct[#MyStruct#]; name=MyStruct
// COMPLETE: End completions
