// RUN: %target-typecheck-verify-swift

func doSomething(onError: ((Error) -> Void)? = nil, onCompletion: (Int) -> Void) { }

func testDoSomething() {
  doSomething { x in
    print(x)
  }

  doSomething(onError: nil) { x in
    print(x)
  }

  doSomething { e in
    print(e)
  } onCompletion: { x in
    print(x)
  }
}

func trailingClosures(
  arg1: () -> Void = {},
  arg2: () -> Void,
  arg3: () -> Void = {}
) {}

func testTrailingClosures() {
  trailingClosures { print("Hello!") }
  trailingClosures { print("Hello,") } arg3: { print("world!") }
}
