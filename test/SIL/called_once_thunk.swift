// RUN: %target-swift-frontend %s \
// RUN: -emit-sil \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute

struct Big {
  var a, b, c, d: Int
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

func genericMakeCalledOnce<T>(_ f: @escaping (T) -> Void) -> @called(once) (T) -> Void {
  return f
}

func consumeCalledOnce(_ f: @called(once) (Int) -> Void) {
  f(42)
}

func makeCalledOnce(_ f: @escaping (Int) -> Void) -> @called(once) (Int) -> Void {
  return identity(f)
}

func testCallOnceThroughThunk() {
  let f = makeCalledOnce { x in print(x) }
  f(1) // Ok
}

func testNeverCalledThroughThunk() {
  let f = makeCalledOnce { x in print(x) } // Ok (`@called(once)` has "at most" semantics)
  _ = f
}

func testDoubleCallThroughThunk() {
  let f = makeCalledOnce { x in print(x) } // expected-error {{'f' consumed more than once}}
  f(1) // expected-note {{consumed here}}
  f(2) // expected-note {{consumed again here}}
}

func testGenericBodyThunkDoubleCall() {
  let f = genericMakeCalledOnce { (x: Int) in print(x) } // expected-error {{'f' consumed more than once}}
  f(1) // expected-note {{consumed here}}
  f(2) // expected-note {{consumed again here}}
}

func testCallOnceThroughThunkAtParameterBoundary(_ f: @escaping (Int) -> Void) {
  consumeCalledOnce(identity(f)) // Ok
}
