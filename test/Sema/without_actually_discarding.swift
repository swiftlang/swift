// RUN: %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions

// REQUIRES: swift_feature_OnceFunctions

func consumeAll(_ f: consuming @once () -> Void,
                _ g: consuming @escaping @once () -> Int) -> Int {
  // The body receives an at-most-once function.
  withoutActuallyDiscarding(f) { (h: consuming @callable(once) () -> Void) in }
  withoutActuallyDiscarding(f) { h in h() }

  // The escaping-ness of the argument carries over to the body parameter.
  return withoutActuallyDiscarding(g) { h in
    store(h)
    return 0
  }
}

func store(_ f: consuming @escaping @callable(once) () -> Int) {}

func result(_ f: consuming @once () -> Void) -> Int {
  withoutActuallyDiscarding(f) { _ in 42 }
}

func throwing(_ f: consuming @once () -> Void) throws {
  struct E: Error {}
  try withoutActuallyDiscarding(f) { _ in throw E() }
}

struct TypedError: Error {}

func typedThrowing(_ f: consuming @once () -> Void) throws(TypedError) {
  try withoutActuallyDiscarding(f) { (_) throws(TypedError) in throw TypedError() }
}

func generic<T>(_ f: consuming @once () -> T) -> Int {
  withoutActuallyDiscarding(f) { _ in 0 }
}

// Only an exactly-once function can be discarded.

func plain(_ f: () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@once' function, but argument has type '() -> Void'}}
}

func atMostOnce(_ f: consuming @callable(once) () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@once' function, but argument has type '@callable(once) () -> Void'}}
}

func notAFunction(_ x: Int) {
  withoutActuallyDiscarding(x) { (_: Int) in }
  // expected-error@-1 {{'withoutActuallyDiscarding' requires a '@once' function, but argument has type 'Int'}}
}
