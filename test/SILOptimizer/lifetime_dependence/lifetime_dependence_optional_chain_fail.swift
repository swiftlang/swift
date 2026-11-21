// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -strict-memory-safety \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

// Rejected forms of Optional chains with a non-Escapable result.

struct View: ~Escapable {
  var p: Int

  @_lifetime(borrow base)
  init(_ base: borrowing Base) { p = 0 }

  @_lifetime(borrow base)
  init(_ base: borrowing NoncopyableBase) { p = 0 }
}

@_addressableForDependencies
struct Base {
  var id: String

  var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

@_addressableForDependencies
struct NoncopyableBase: ~Copyable {
  var id: String

  var view: View {
    @_lifetime(borrow self)
    borrowing get { View(self) }
  }
}

final class Holder {
  var base: Base? = nil
}

struct Computed {
  var stored: Base?
  var computed: Base? { stored }
}

struct Yielded {
  var stored: Base?
  var inner: Base? {
    _read { yield stored }
  }
}

var global: Base? = nil

struct GlobalOuter {
  var inner: Base?
}

var globalOuter = GlobalOuter()

struct Wrapper {
  var holder: Holder
}

struct ComputedBaseHolder {
  var stored: Base
  var computed: Base { stored }
}

final class ClassBaseHolder {
  var base = Base(id: "")
}

struct ClassBaseHolderWrapper {
  var holder: ClassBaseHolder
}

func f(_ s: View?) {}

func make() -> Base? { Base(id: "") }

// The base is a temporary, to be destroyed after the chain.
func temporary() {
  f(make()?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// The access to the stored property begins inside the chain.
func classHop(_ w: Wrapper?) {
  f(w?.holder.base?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'base'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// Class storage isn't handled.
func classProperty(_ h: Holder) {
  f(h.base?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'base'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// Global storage isn't handled.
func globalOperand() {
  f(global?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'global'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func globalMember() {
  f(globalOuter.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'globalOuter'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// A computed property produces a temporary value.
func computedOperand(_ c: Computed) {
  f(c.computed?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func computedMember(_ c: Computed?) {
  f(c?.computed?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// A computed property after the binding produces a temporary value.
func computedAfterBinding(_ c: ComputedBaseHolder?) {
  f(c?.computed.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// Class storage after the binding isn't handled.
func classPropertyAfterBinding(_ w: ClassBaseHolderWrapper?) {
  f(w?.holder.base.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'base'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// A parenthesized chain is evaluated on its own, so its value is a temporary.
func parenthesizedChain(_ o: GlobalOuter?) { // expected-note {{it depends on the lifetime of this parent value}}
  f((o?.inner)?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{this use of the lifetime-dependent value is out of scope}}
}

func yieldedOperand(_ y: Yielded) {
  f(y.inner?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of this parent value}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

// The view holds a read access to the base, prevents consuming.
func noncopyableConsume(_ b: consuming NoncopyableBase?) {
  let v = b?.view // expected-note {{conflicting access is here}}
  _ = consume b // expected-error {{overlapping accesses to 'b', but modification requires exclusive access}}
  f(v)
}

// The view holds a read access to the base, prevents mutations.
func noncopyableOverlap(_ b: inout NoncopyableBase?) {
  g(b?.view, &b) // expected-error {{overlapping accesses to 'b', but modification requires exclusive access}}
  // expected-note @-1 {{conflicting access is here}}
}

func g(_ v: borrowing View?, _ b: inout NoncopyableBase?) {}
