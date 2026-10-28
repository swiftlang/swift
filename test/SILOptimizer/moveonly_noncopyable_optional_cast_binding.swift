// RUN: %target-swift-emit-sil %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// `if let v = x as? T` over a non-`Copyable` subject consumes the subject only
// on success (the same operation as `if case let v as T = x`), so a chain of
// attempts type-checks and a failed attempt leaves the subject usable.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Small: P, ~Copyable { var tag: Int }
struct Unrelated: Q, ~Copyable { var tag: Int }

func ifLetChain(_ box: consuming any P & ~Copyable) -> Int {
  if let u = box as? Unrelated { return u.tag } // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'Unrelated' always fails}}
  if let s = box as? Small { return s.tag }
  return -1
}

func guardLetChain(_ box: consuming any P & ~Copyable) -> Int {
  guard let u = box as? Unrelated else { // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'Unrelated' always fails}}
    guard let s = box as? Small else { return -1 }
    return s.tag
  }
  return u.tag
}

func whileLet(_ box: consuming any P & ~Copyable) -> Int {
  while let s = box as? Small { return s.tag }
  return -1
}

func ifCaseSome(_ box: consuming any P & ~Copyable) -> Int {
  if case let s? = box as? Small { return s.tag }
  return -1
}

func localVar() -> Int {
  var box: any P & ~Copyable = Small(tag: 1)
  box = Small(tag: 2)
  if let u = box as? Unrelated { return u.tag } // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'Unrelated' always fails}}
  if let s = box as? Small { return s.tag }
  return -1
}

func localLet() -> Int {
  let box: any P & ~Copyable = Small(tag: 1)
  if let u = box as? Unrelated { return u.tag } // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'Unrelated' always fails}}
  if let s = box as? Small, s.tag > 0 { return s.tag }
  return -1
}

func inoutParam(_ box: inout any P & ~Copyable) -> Int {
  if let s = box as? Small { box = Small(tag: 0); return s.tag }
  return -1
}

func optionalSubject(_ box: consuming (any P & ~Copyable)?) -> Int {
  if let b = box, let s = b as? Small { return s.tag }
  return -1
}

// A successful cast consumes the subject.
func useAfterSuccess(_ box: consuming any P & ~Copyable) -> Int {
  // expected-error @-1 {{'box' consumed more than once}}
  var r = 0
  if let s = box as? Small { r = s.tag } // expected-note {{consumed here}}
  _ = box // expected-note {{consumed again here}}
  return r
}

// A borrowed subject cannot be consumed by a binding cast. Supporting it is
// the deferred borrowed-payload work.
func borrowedSubject(_ box: borrowing any P & ~Copyable) -> Int {
  // expected-error @-1 {{'box' is borrowed and cannot be consumed}}
  if let s = box as? Small { return s.tag } // expected-note {{consumed here}}
  return -1
}
