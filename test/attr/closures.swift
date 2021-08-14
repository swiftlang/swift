// RUN: %target-typecheck-verify-swift -swift-version 5

func testNonacceptedClosures() {
  let fn = { @usableFromInline in // expected-error{{'usableFromInline' is not supported on a closure}}
    "hello"
  }

  let fn2: (Int) -> Int = { @usableFromInline x in  // expected-error{{'usableFromInline' is not supported on a closure}}
    print("hello")
    return x
  }

  _ = fn
  _ = fn2
}
