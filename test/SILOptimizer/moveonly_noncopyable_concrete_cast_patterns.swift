// RUN: %target-swift-emit-sil %s -target %target-future-triple -sil-verify-all -verify -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// Similar to moveonly_noncopyable_existential_cast_patterns.swift
// but for a general noncopyable subject, not just an existential.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Small: P, ~Copyable { var tag: Int }
enum NCEnum: P, ~Copyable { case one(Int), two }

// MARK: - Consumable subjects: these must compile, and must preserve the
// subject across a failed (non-final) attempt in the chain.
//
// The second `if case let` in each of these verifies that the source
// is not consumed on failure of the first one.

func concreteStructParam(_ box: consuming Small) -> Int {
  if case let q as (Q & ~Copyable) = box { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as Small = box { return p.tag }
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'as' test is always true}}
  return -2
}

func concreteEnumParam(_ box: consuming NCEnum) -> Int {
  if case let q as (Q & ~Copyable) = box { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case is (P & ~Copyable) = box { return 1 }
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'is' test is always true}}
  // expected-warning @-3 {{constraint that suppresses conformance requires 'any'}}
  return -2
  // expected-warning @-1 {{will never be executed}}
}

func concreteLocalLet() -> Int {
  let box = Small(tag: 1)
  if case let q as (Q & ~Copyable) = box { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as Small = box { return p.tag }
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'as' test is always true}}
  return -2
}

func concreteLocalVar() -> Int {
  var box = Small(tag: 2)
  _ = consume box
  box = Small(tag: 2)
  if case let q as (Q & ~Copyable) = box { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as Small = box { return p.tag }
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'as' test is always true}}
  return -2
}

struct VarHolder: ~Copyable {
  var inner: Small
}

func concreteVarStoredProperty(_ h: consuming VarHolder) -> Int {
  if case let q as (Q & ~Copyable) = h.inner { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as Small = h.inner { return p.tag }
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'as' test is always true}}
  return -2
}

func genericParam<S: ~Copyable>(_ box: consuming S) -> Bool {
  if case let q as (Q & ~Copyable) = box { _ = q; return false }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case is (P & ~Copyable) = box { return true }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  return false
}

// MARK: - Non-consumable subjects: these must diagnose, not crash. Same rules
// as the existential case, since findConsumableStorageReference doesn't look
// at the subject's type at all -- only at where it's stored.

func concreteBorrowingParam(_ box: borrowing Small) -> Int {
  // expected-error @-1 {{'box' is borrowed and cannot be consumed}}
  if case let p as (P & ~Copyable) = box { _ = p; return box.tag } // expected-note {{consumed here}}
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning @-2 {{'if' condition is always true}}
  return -1
}

let globalSmall = Small(tag: 3)

func concreteGlobalLet() -> Int {
  if case let p as (P & ~Copyable) = globalSmall { _ = p; return globalSmall.tag }
  // expected-error @-1 {{cannot consume non-'Copyable' stored property 'globalSmall' that is global}}
  // expected-warning @-2 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning @-3 {{'if' condition is always true}}
  return -1
}

struct LetHolder: ~Copyable {
  let inner: Small
}

func concreteLetStoredProperty(_ h: consuming LetHolder) -> Int {
  // expected-error @-1 {{'h' consumed more than once}}
  if case let q as (Q & ~Copyable) = h.inner { _ = q; return -1 } // expected-note {{consumed here}}
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as Small = h.inner { return p.tag } // expected-note {{consumed again here}}
  // expected-warning @-1 {{'if' condition is always true}}
  // expected-warning @-2 {{'as' test is always true}}
  return -2
}

// MARK: - Reusing original value is use-after-consume

func concreteRereadAfterCoercionFallback(_ box: consuming Small) -> Int {
  // expected-error @-1 {{'box' used after consume}}
  if case let p as (P & ~Copyable) = box { _ = p; return box.tag }
  // expected-note @-1 {{consumed here}}
  // expected-note @-2 {{used here}}
  // expected-warning @-3 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning @-4 {{'if' condition is always true}}
  return -1
}

func concreteRereadAfterChainedCoercionFallback(_ box: consuming Small) -> Int {
  // expected-error @-1 {{'box' used after consume}}
  if case let q as (Q & ~Copyable) = box { _ = q; return -1 }
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  if case let p as (P & ~Copyable) = box { _ = p; return box.tag } // expected-note {{consumed here}} expected-note {{used here}}
  // expected-warning @-1 {{constraint that suppresses conformance requires 'any'}}
  // expected-warning @-2 {{'if' condition is always true}}
  return -2
}
