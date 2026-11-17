// RUN: %target-swift-emit-sil %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// `switch consume x { case let v as T: ... }` over a non-`Copyable` subject.
// A consuming switch tests the cases under a borrow and binds the payload only
// in the chosen case, so a failed test leaves the subject for the next case.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Small: P, ~Copyable { var tag: Int }
struct Unrelated: Q, ~Copyable { var tag: Int }
enum Wrapper: ~Copyable { case one(any P & ~Copyable), two(Int) }

func existentialParam(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case let u as Unrelated: return u.tag
  // expected-warning @-1 {{cast from 'any P & ~Copyable' to unrelated type 'Unrelated' always fails}}
  case let s as Small: return s.tag
  default: return -1
  }
}

func existentialLocalVar() -> Int {
  var box: any P & ~Copyable = Small(tag: 1)
  box = Small(tag: 2)
  switch consume box {
  case let s as Small: return s.tag
  default: return -1
  }
}

func optionalPayload(_ box: consuming (any P & ~Copyable)?) -> Int {
  switch consume box {
  case .some(let s as Small): return s.tag
  case .some: return -2
  case .none: return -1
  }
}

func enumPayload(_ box: consuming Wrapper) -> Int {
  switch consume box {
  case .one(let s as Small): return s.tag
  case .one: return -2
  case .two(let n): return n
  }
}

func genericParam<T: ~Copyable>(_ box: consuming T) -> Int {
  switch consume box {
  case let s as Small: return s.tag
  default: return -1
  }
}

// A statically satisfied cast matches unconditionally.
func concreteIdentity(_ box: consuming Small) -> Int {
  switch consume box {
  case let s as Small: return s.tag
  // expected-warning @-1 {{'as' test is always true}}
  }
}

// The switch consumes the subject.
func useAfterSwitch(_ box: consuming any P & ~Copyable) -> Int {
  // expected-error @-1 {{'box' consumed more than once}}
  var r = 0
  switch consume box { // expected-note {{consumed here}}
  case let s as Small: r = s.tag
  default: r = -1
  }
  _ = box // expected-note {{consumed again here}}
  return r
}
