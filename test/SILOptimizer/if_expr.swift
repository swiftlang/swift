// RUN: %target-swift-emit-sil -verify %s -o /dev/null

func takesGenericReturningFn<R>(_ fn: () -> R) {}

func testOuterClosureReturn() {
  takesGenericReturningFn {
    if .random() {
      return
    } else {
      ()
    }
  }
}

func testNeverToVoid() {
  takesGenericReturningFn {
    if .random() { // This does not turn into an expression due to the 'do'.
      fatalError()
    } else {
      do {}
    }
  }
}
