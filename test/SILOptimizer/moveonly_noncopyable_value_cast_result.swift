// RUN: %target-swift-emit-sil %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting
// RUN: %target-swift-emit-sil -O %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// A value `x as? T` whose result is non-`Copyable` is cast into a temporary
// rather than straight into the variable it initializes, so the move checker
// sees an ordinary initialization of that variable. Consuming operands work;
// operands that can't be consumed get the ordinary diagnostic.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}
struct NC: P, ~Copyable { var tag: Int }

var global: (any P & ~Copyable)? = nil

func consumingConcrete(_ b: consuming any P & ~Copyable) -> Int {
  let x = b as? NC
  if let n = x { return n.tag }
  return 0
}

func consumingExistential(_ b: consuming any P & ~Copyable) -> Bool {
  let x = b as? any Q & ~Copyable
  if let q = x { _ = q; return true }
  return false
}

func consumingOptional(_ o: consuming (any P & ~Copyable)?) -> Bool {
  let x = o as? any Q & ~Copyable
  if let q = x { _ = q; return true }
  return false
}

func borrowedConcrete(_ b: borrowing any P & ~Copyable) {
  // expected-error@-1 {{'b' is borrowed and cannot be consumed}}
  let x = b as? NC // expected-note {{consumed here}}
  _ = x
}

func borrowedOptionalExistential(_ o: borrowing (any P & ~Copyable)?) -> Bool {
  // expected-error@-1 {{'o' is borrowed and cannot be consumed}}
  if let q = o as? any Q & ~Copyable { _ = q; return true } // expected-note {{consumed here}}
  return false
}

func globalOptionalExistential() -> Bool {
  if let q = global as? any Q & ~Copyable { _ = q; return true }
  // expected-error@-1 {{cannot consume non-'Copyable' stored property 'global' that is global}}
  return false
}
