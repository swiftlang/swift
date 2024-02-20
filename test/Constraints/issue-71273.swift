// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/71273

func bar<R>(_ fn: () -> R) {}

// Make sure we don't error here.
func testLocalFn() {
  bar() {
    func foo() -> Int { return 0 }
    return ()
  }
}

func testLocalBinding() {
  bar() {
    let _ = if .random() { return () } else { 0 }
    // expected-error@-1 {{cannot 'return' in 'if' when used as expression}}
    return ()
  }
}
