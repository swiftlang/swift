// RUN: %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions

// REQUIRES: swift_feature_OnceFunctions

func consumeAll(_ f: consuming @once () -> Void,
                _ g: consuming @escaping @once () -> Int) -> Int {
  // The body receives an at-most-once function.
  withoutActuallyDeiniting(f) { (h: consuming @callable(once) () -> Void) in }
  withoutActuallyDeiniting(f) { h in h() }

  // The escaping-ness of the argument carries over to the body parameter.
  return withoutActuallyDeiniting(g) { h in
    store(h)
    return 0
  }
}

func store(_ f: consuming @escaping @callable(once) () -> Int) {}

func result(_ f: consuming @once () -> Void) -> Int {
  withoutActuallyDeiniting(f) { _ in 42 }
}

func throwing(_ f: consuming @once () -> Void) throws {
  struct E: Error {}
  try withoutActuallyDeiniting(f) { _ in throw E() }
}

struct TypedError: Error {}

func typedThrowing(_ f: consuming @once () -> Void) throws(TypedError) {
  try withoutActuallyDeiniting(f) { (_) throws(TypedError) in throw TypedError() }
}

func generic<T>(_ f: consuming @once () -> T) -> Int {
  withoutActuallyDeiniting(f) { _ in 0 }
}

// Only an exactly-once function can be discarded.

func plain(_ f: () -> Void) {
  withoutActuallyDeiniting(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDeiniting' requires a '@once' function, but argument has type '() -> Void'}}
}

func atMostOnce(_ f: consuming @callable(once) () -> Void) {
  withoutActuallyDeiniting(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDeiniting' requires a '@once' function, but argument has type '@callable(once) () -> Void'}}
}

func notAFunction(_ x: Int) {
  withoutActuallyDeiniting(x) { (_: Int) in }
  // expected-error@-1 {{'withoutActuallyDeiniting' requires a '@once' function, but argument has type 'Int'}}
}
