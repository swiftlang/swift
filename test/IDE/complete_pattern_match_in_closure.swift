// RUN: %batch-code-completion

func run(execute workItem: Int) {}
func run(execute work: () -> Void) {}

func test(myVar: Int) {
  run {
    guard let data = #^COMPLETE^# else {
      return
    }
  }
}

// COMPLETE: Decl[LocalVar]/Local:               myVar[#Int#]; name=myVar
