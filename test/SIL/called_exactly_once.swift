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
