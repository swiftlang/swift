// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute

// REQUIRES: swift_feature_CalledAttribute

func consumeAll(_ f: consuming @called(exactlyOnce) () -> Void,
                _ g: consuming @escaping @called(exactlyOnce) () -> Int) -> Int {
  // The body receives an at-most-once function.
  withoutActuallyDiscarding(f) { (h: consuming @called(atMostOnce) () -> Void) in }
  withoutActuallyDiscarding(f) { h in h() }

  // The escaping-ness of the argument carries over to the body parameter.
  return withoutActuallyDiscarding(g) { h in
    store(h)
    return 0
  }
}

func store(_ f: consuming @escaping @called(atMostOnce) () -> Int) {}

func result(_ f: consuming @called(exactlyOnce) () -> Void) -> Int {
  withoutActuallyDiscarding(f) { _ in 42 }
}

func throwing(_ f: consuming @called(exactlyOnce) () -> Void) throws {
  struct E: Error {}
  try withoutActuallyDiscarding(f) { _ in throw E() }
}

struct TypedError: Error {}

func typedThrowing(_ f: consuming @called(exactlyOnce) () -> Void) throws(TypedError) {
  try withoutActuallyDiscarding(f) { (_) throws(TypedError) in throw TypedError() }
}

func generic<T>(_ f: consuming @called(exactlyOnce) () -> T) -> Int {
  withoutActuallyDiscarding(f) { _ in 0 }
}

// Only an exactly-once function can be discarded.

func plain(_ f: () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@called(exactlyOnce)' function, but argument has type '() -> Void'}}
}

func atMostOnce(_ f: consuming @called(atMostOnce) () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@called(exactlyOnce)' function, but argument has type '@called(atMostOnce) () -> Void'}}
}

func notAFunction(_ x: Int) {
  withoutActuallyDiscarding(x) { (_: Int) in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@called(exactlyOnce)' function, but argument has type 'Int'}}
}
