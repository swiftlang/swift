// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -emit-sil -verify -enable-sil-opaque-values %s

// `type(of:)` only reads the type of its operand, so it must not consume it.

struct NC: ~Copyable {
  var x: Int = 0
  deinit {}
}

struct Wrapper: ~Copyable {
  var nc = NC()
  var copyable = 0
}

func consume<T: ~Copyable>(_: consuming T) {}
func borrow<T: ~Copyable>(_: borrowing T) {}

// MARK: concrete noncopyable operands

func concreteConsumingParam(_ v: consuming NC) {
  _ = type(of: v)
  consume(v)
}

func concreteBorrowingParam(_ v: borrowing NC) {
  _ = type(of: v)
  borrow(v)
}

func concreteInoutParam(_ v: inout NC) {
  _ = type(of: v)
  v.x = 1
}

func concreteLocalVar() {
  var v = NC()
  _ = type(of: v)
  v.x = 1
  consume(v)
}

func concreteLocalLet() {
  let v = NC()
  _ = type(of: v)
  consume(v)
}

func concreteStoredProperty(_ w: consuming Wrapper) {
  _ = type(of: w.nc)
  consume(w)
}

// MARK: generic noncopyable operands
//
// These are the interesting cases: an address-only operand used to be copied
// into a temporary to form an rvalue, which counts as a consume.

func genericConsumingParam<T: ~Copyable>(_ v: consuming T) {
  _ = type(of: v)
  consume(v)
}

func genericBorrowingParam<T: ~Copyable>(_ v: borrowing T) {
  _ = type(of: v)
  borrow(v)
}

func genericInoutParam<T: ~Copyable>(_ v: inout T) {
  _ = type(of: v)
  borrow(v)
}

func genericLocalVar<T: ~Copyable>(_ v: consuming T, _ other: consuming T) {
  var local = consume v
  _ = type(of: local)
  borrow(local)
  local = consume other
  consume(local)
}

func genericLocalLet<T: ~Copyable>(_ v: consuming T) {
  let local = consume v
  _ = type(of: local)
  consume(local)
}

// Repeated queries must not each consume the operand.
func genericRepeated<T: ~Copyable>(_ v: consuming T) {
  _ = type(of: v)
  _ = type(of: v)
  _ = type(of: v)
  consume(v)
}

// MARK: noncopyable existential operands
//
// These go through `existential_metatype` rather than `value_metatype`, but it
// likewise only reads its operand.

protocol P: ~Copyable {}

struct ConformsToP: ~Copyable, P {
  deinit {}
}

func existentialConsumingParam(_ box: consuming any P & ~Copyable) {
  _ = type(of: box)
  consume(box)
}

func existentialBorrowingParam(_ box: borrowing any P & ~Copyable) {
  _ = type(of: box)
  borrow(box)
}

func existentialInoutParam(_ box: inout any P & ~Copyable) {
  _ = type(of: box)
  box = ConformsToP()
}

// Local `var`/`let` bindings of noncopyable existential type are omitted here:
// any use of one crashes the move-only address checker even without
// `type(of:)`, so they can't be tested yet. The parameter cases above and below
// exercise the same `existential_metatype` emission path.

func existentialRepeated(_ box: consuming any P & ~Copyable) {
  _ = type(of: box)
  _ = type(of: box)
  consume(box)
}

func existentialStillDiagnosed(_ box: consuming any P & ~Copyable) { // expected-error {{'box' used after consume}}
  consume(box) // expected-note {{consumed here}}
  _ = type(of: box) // expected-note {{used here}}
}

// MARK: noncopyable existential metatypes
//
// `type(of:)` on a noncopyable existential yields the existential metatype
// `any (P & ~Copyable).Type`, which is itself copyable and trivial.

func existentialMetatypeResult(
  _ box: borrowing any P & ~Copyable
) -> any (P & ~Copyable).Type {
  return type(of: box)
}

// The metatype outlives the borrow of the value it was derived from.
func existentialMetatypeOutlivesOperand(_ box: consuming any P & ~Copyable) {
  let m = type(of: box)
  consume(box)
  _ = m
}

// An existential metatype is copyable, so `type(of:)` on one is unrestricted.
func metatypeOfExistentialMetatype(_ box: borrowing any P & ~Copyable) {
  let m: any (P & ~Copyable).Type = type(of: box)
  _ = type(of: m)
  _ = m
}

func genericMetatypeOutlivesOperand<T: ~Copyable>(_ v: consuming T) {
  let m = type(of: v)
  consume(v)
  _ = m
}

// MARK: consuming the operand is still diagnosed when it really happens

func stillDiagnosed<T: ~Copyable>(_ v: consuming T) { // expected-error {{'v' used after consume}}
  consume(v) // expected-note {{consumed here}}
  _ = type(of: v) // expected-note {{used here}}
}
