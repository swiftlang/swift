// RUN: %target-typecheck-verify-swift

func doSomething(onError: ((Error) -> Void)? = nil, onCompletion: (Int) -> Void) { }

func testDoSomething() {
  doSomething { x in
    print(x)
  }

  doSomething(onError: nil) { x in
    print(x)
  }
}
