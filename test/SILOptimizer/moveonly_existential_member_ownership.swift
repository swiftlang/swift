// RUN: %target-swift-frontend -emit-sil -verify %s
// RUN: %target-swift-frontend -emit-sil -verify -swift-version 6 %s

// Calling a member on a noncopyable existential held in *storage* (a parameter,
// a `var`, or a stored property) used to emit the existential's opening as an
// rvalue: SILGen copied the existential out of its formal access into a
// temporary and opened that. A copy of a noncopyable value is not something the
// move-only checker can eliminate, so a `consuming` member produced "copy of
// noncopyable typed value. This is a compiler bug", and a `borrowing` member
// was mistaken for a consume.
//
// Now the storage is borrowed in place under the enclosing formal access and
// opened directly, as already happened for an existential that isn't behind an
// access at all (a local `let`, or an rvalue).

protocol P: ~Copyable {
  consuming func con()
  borrowing func bor()
  mutating func mut()
  consuming func conSelf() -> Self
}

struct S: P, ~Copyable {
  var v = 0
  consuming func con() {}
  borrowing func bor() {}
  mutating func mut() {}
  consuming func conSelf() -> S { S() }
  deinit {}
}

func takeC<T: ~Copyable>(_: consuming T) {}
func takeB<T: ~Copyable>(_: borrowing T) {}
func makeExistential() -> any P & ~Copyable { S() }

// MARK: consuming members on storage

func consumingOnConsumingParam(_ x: consuming any P & ~Copyable) {
  x.con()
}

func consumingOnLocalVar() {
  var x: any P & ~Copyable = S()
  x = S()
  x.con()
}

func consumingAsGenericArgument(_ x: consuming any P & ~Copyable) {
  takeC(x)
}

// MARK: borrowing members must not be treated as consumes

func repeatedBorrowOnConsumingParam(_ x: consuming any P & ~Copyable) {
  x.bor()
  x.bor()
}

func borrowThenConsume(_ x: consuming any P & ~Copyable) {
  x.bor()
  x.con()
}

func borrowOnBorrowingParam(_ x: borrowing any P & ~Copyable) {
  x.bor()
  takeB(x)
}

// A borrowing member on an `inout` existential is not a consume, so it must not
// require reinitialization of the parameter.
func borrowOnInoutParam(_ x: inout any P & ~Copyable) {
  x.bor()
}

func mutatingOnInoutParam(_ x: inout any P & ~Copyable) {
  x.mut()
}

// MARK: the existential in a stored property

struct Wrapper: ~Copyable {
  var box: any P & ~Copyable
}

func consumingOnStoredProperty(_ w: consuming Wrapper) {
  w.box.con()
}

// MARK: existentials not behind an access still work

func consumingOnLocalLet() {
  let x: any P & ~Copyable = S()
  x.con()
}

func consumingOnRValue() {
  makeExistential().con()
}

func consumingAfterExplicitConsume(_ x: consuming any P & ~Copyable) {
  let y = consume x
  y.con()
}

func consumingViaTemporary(_ x: inout any P & ~Copyable) {
  let y = x.conSelf()
  x = y
}

// MARK: ownership diagnostics are still reported, and are accurate

func doubleConsumeStillDiagnosed(_ x: consuming any P & ~Copyable) { // expected-error {{'x' consumed more than once}}
  x.con() // expected-note {{consumed here}}
  x.con() // expected-note {{consumed again here}}
}

func useAfterConsumeStillDiagnosed(_ x: consuming any P & ~Copyable) { // expected-error {{'x' used after consume}}
  x.con() // expected-note {{consumed here}}
  x.bor() // expected-note {{used here}}
}

// MARK: known limitation
//
// `x = x.conSelf()` logically consumes (deinitializes) `x` before assigning the
// new value (reinitializing).  So you might expect this to work.  But in
// practice we hold the LHS of the assignment while evaluating the RHS, which
// violates exclusivity. In theory, this might be fixable but there's an easy
// workaround (explicit temporary) so it's probably not worth it.

func reassignFromConsumingMemberInout(_ x: inout any P & ~Copyable) {
  x = x.conSelf() // expected-error {{overlapping accesses to 'x', but modification requires exclusive access}}
  // expected-note@-1 {{conflicting access is here}}
}

func reassignFromConsumingMemberVar() {
  var x: any P & ~Copyable = S()
  x = x.conSelf() // expected-error {{overlapping accesses to 'x', but modification requires exclusive access}}
  // expected-note@-1 {{conflicting access is here}}
}
