// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -emit-sil -verify -enable-sil-opaque-values %s

// Local bindings of noncopyable existential type used to crash the move-only
// address checker with "Attempted to set out-of-bounds range!".
//
// An existential is an opaque leaf of the type tree: `TypeSubElementCount` of
// `any P & ~Copyable` is 1. But `SubElementOffset::computeForAddress` looks
// through `init_existential_addr`/`open_existential_addr` without advancing the
// offset, while the *width* of the affected leaf range was computed from the
// projection's own type -- the concrete payload. Any payload with more than one
// leaf therefore produced a range wider than the whole type tree it sits in.

protocol P: ~Copyable {}

func consume<T: ~Copyable>(_: consuming T) {}
func borrow<T: ~Copyable>(_: borrowing T) {}

// A payload with two leaves: one stored property plus the deinit bit.
struct TwoLeaves: ~Copyable, P {
  var x = 0
  deinit {}
}

// A payload with one leaf, which happened not to crash.
struct OneLeaf: ~Copyable, P {
  var x = 0
}

// A payload whose only leaf is the deinit bit.
struct DeinitOnly: ~Copyable, P {
  deinit {}
}

// Several leaves from stored properties alone.
struct ThreeFields: ~Copyable, P {
  var x = 0
  var y = 0
  var z = 0
}

// MARK: local `let` bindings

func localLetTwoLeaves() {
  let box: any P & ~Copyable = TwoLeaves()
  consume(box)
}

func localLetOneLeaf() {
  let box: any P & ~Copyable = OneLeaf()
  consume(box)
}

func localLetDeinitOnly() {
  let box: any P & ~Copyable = DeinitOnly()
  consume(box)
}

func localLetThreeFields() {
  let box: any P & ~Copyable = ThreeFields()
  consume(box)
}

func localLetBorrowed() {
  let box: any P & ~Copyable = TwoLeaves()
  borrow(box)
}

// MARK: local `var` bindings

func localVarReassigned() {
  var box: any P & ~Copyable = TwoLeaves()
  box = ThreeFields()
  consume(box)
}

func localVarBorrowedThenConsumed() {
  var box: any P & ~Copyable = TwoLeaves()
  borrow(box)
  box = DeinitOnly()
  consume(box)
}

// MARK: the existential nested inside another noncopyable aggregate
//
// Here the existential is not at offset 0, so an over-wide range would claim
// leaves belonging to the following field rather than just running off the end.

struct Wrapper: ~Copyable {
  var before: NoncopyableField
  var box: any P & ~Copyable
  var after: NoncopyableField
}

struct NoncopyableField: ~Copyable {
  var v = 0
  deinit {}
}

func nestedExistential(_ w: consuming Wrapper) {
  borrow(w)
  consume(w)
}

func nestedExistentialFieldReassigned(_ w: consuming Wrapper) {
  var w = consume w
  w.box = ThreeFields()
  consume(w)
}

// MARK: leaf ranges around the nested existential must be precise
//
// The existential is exactly one leaf, distinct from its siblings. An over-wide
// range would make consuming `box` look like it also consumed `after`.

func partialConsumeOfBoxLeavesSiblingsAlone(_ w: consuming Wrapper) {
  consume(w.box)
  borrow(w.before)
  borrow(w.after)
}

func partialConsumeOfSiblingLeavesBoxAlone(_ w: consuming Wrapper) {
  consume(w.after)
  borrow(w.box)
  borrow(w.before)
}

// MARK: diagnostics must still be correct, not merely non-crashing

func doubleConsumeOfBoxStillDiagnosed(_ w: consuming Wrapper) { // expected-error {{'w' consumed more than once}}
  consume(w.box) // expected-note {{consumed here}}
  consume(w.box) // expected-note {{consumed again here}}
}

func useOfBoxAfterConsumeStillDiagnosed(_ w: consuming Wrapper) { // expected-error {{'w' used after consume}}
  consume(w.box) // expected-note {{consumed here}}
  borrow(w.box) // expected-note {{used here}}
}

func stillDiagnosedConsumedTwice() {
  let box: any P & ~Copyable = TwoLeaves() // expected-error {{'box' consumed more than once}}
  consume(box) // expected-note {{consumed here}}
  consume(box) // expected-note {{consumed again here}}
}

func stillDiagnosedUseAfterConsume() {
  let box: any P & ~Copyable = ThreeFields() // expected-error {{'box' used after consume}}
  consume(box) // expected-note {{consumed here}}
  borrow(box) // expected-note {{used here}}
}
