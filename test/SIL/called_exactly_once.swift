// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute

func makeClosure() -> @called(exactlyOnce) () -> Void {
  return {}
}

func testCallOnce(_ f: @called(exactlyOnce) () -> Void) {
  f()
}

func testCalledInBothBranches(cond: Bool, _ f: @called(exactlyOnce) () -> Void) {
  if cond {
    f()
  } else {
    f()
  }
}

func testMoveThenCall(_ f: @called(exactlyOnce) () -> Void) {
  let g = f
  g()
}

func testImmediateCallOnReturnValue() {
  makeClosure()() // Ok
}

func passThroughParam(fn: @called(exactlyOnce) () -> Void) {
  testCallOnce(fn) // Ok
}

func testDoubleCall(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

func testCalledInLoop(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed in a loop}}
  for _ in 0..<10 {
    f() // expected-note {{consumed here}}
  }
}

func testCallThenForward(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  testCallOnce(f) // expected-note {{consumed again here}}
}

// MARK: - Captures

// A closure that might run more than once can't consume what it captures, and
// any use of a `@called(exactlyOnce)` value consumes it.

func takesPlain(_: () -> Void) {}

func capturedByEscapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  let g = { f() } // expected-note {{consumed here}}
  g()
}

func capturedByNonescapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  takesPlain { f() } // expected-note {{consumed here}}
}

func capturedByLocalFunction(_ f: @escaping @called(exactlyOnce) () -> Void) {
  func local() {
    f() // expected-error {{noncopyable 'f' cannot be consumed when captured by an escaping closure or borrowed by a non-Escapable type}}
  }
  local()
}

func capturedThroughPlainClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  let outer = {
    let inner = { @called(exactlyOnce) in f() } // expected-note {{consumed here}}
    inner()
  }
  outer()
}

// FIXME: [called-once] A `defer` body runs exactly once, so it should be able
// to call a captured `@called(exactlyOnce)` value.
func capturedByDefer(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  defer { f() } // expected-note {{consumed here}}
  print("body")
}
