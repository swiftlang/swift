// RUN: %target-typecheck-verify-swift -swift-version 5

func testNonacceptedClosures() {
  let fn = { @usableFromInline in // expected-error{{attribute @usableFromInline is not supported on a closure}}
    "hello"
  }

  let fn2: (Int) -> Int = { @usableFromInline x in  // expected-error{{attribute @usableFromInline is not supported on a closure}}
    print("hello")
    return x
  }

  _ = fn
  _ = fn2

  // https://github.com/swiftlang/swift/issues/76291
  _ = { (@objc x: Int) in 0 } // expected-error {{'@objc' attribute cannot be applied to this declaration}}
  _ = { (@objc x: Int) in }  // expected-error {{'@objc' attribute cannot be applied to this declaration}}

  _ = { (@objc x: Int) in  // expected-error {{'@objc' attribute cannot be applied to this declaration}}
    print("hello")
    return x
  }
}
