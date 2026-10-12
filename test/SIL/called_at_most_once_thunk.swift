// RUN: %target-swift-frontend %s \
// RUN: -emit-sil \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute

struct Big {
  var a, b, c, d: Int
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

func genericMakeCalledAtMostOnce<T>(_ f: @escaping (T) -> Void) -> @called(atMostOnce) (T) -> Void {
  return f
}

func consumeCalledAtMostOnce(_ f: @called(atMostOnce) (Int) -> Void) {
  f(42)
}

func makeCalledAtMostOnce(_ f: @escaping (Int) -> Void) -> @called(atMostOnce) (Int) -> Void {
  return identity(f)
}

func testCallAtMostOnceThroughThunk() {
  let f = makeCalledAtMostOnce { x in print(x) }
  f(1) // Ok
}

func testNeverCalledThroughThunk() {
  let f = makeCalledAtMostOnce { x in print(x) } // Ok (`@called(atMostOnce)` has "at most" semantics)
  _ = f
}

func testDoubleCallThroughThunk() {
  let f = makeCalledAtMostOnce { x in print(x) } // expected-error {{'f' consumed more than once}}
  f(1) // expected-note {{consumed here}}
  f(2) // expected-note {{consumed again here}}
}

func testGenericBodyThunkDoubleCall() {
  let f = genericMakeCalledAtMostOnce { (x: Int) in print(x) } // expected-error {{'f' consumed more than once}}
  f(1) // expected-note {{consumed here}}
  f(2) // expected-note {{consumed again here}}
}

func testCallAtMostOnceThroughThunkAtParameterBoundary(_ f: @escaping (Int) -> Void) {
  consumeCalledAtMostOnce(identity(f)) // Ok
}
