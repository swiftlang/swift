// RUN: %target-typecheck-verify-swift -swift-version 4

// FIXME: make these errors

func test0(fn: (() -> ()) -> ()) {
  fn { fn {} } // expected-error {{passing a closure which captures a non-escaping function parameter 'fn' to a call to a non-escaping function parameter can allow re-entrant modification of a variable}}
}

func test1(fn: (() -> ()) -> ()) {
  // TODO: infer that this function is noescape from its captures
  func foo() {
    fn { fn {} } // expected-error {{can allow re-entrant modification}}
  }
}

func test2(x: inout Int, fn: (() -> ()) -> ()) {
  func foo(myfn: () -> ()) {
    x += 1
    myfn()
  }

  // Make sure we only complain about calls to noescape parameters.
  foo { fn {} }
}

func test3(fn: (() -> ()) -> ()) {
  { myfn in myfn { fn {} } }(fn) // expected-error {{can allow re-entrant modification}}
}

func test4(fn: (() -> ()) -> ()) {
  // TODO: this should be rejected
  func foo() {
    fn {}
  }

  fn(foo)
}
