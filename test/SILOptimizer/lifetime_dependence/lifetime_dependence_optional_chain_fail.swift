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

var global: Base? = nil

struct GlobalOuter {
  var inner: Base?
}

var globalOuter = GlobalOuter()

struct Wrapper {
  var holder: Holder
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

// A noncopyable base isn't handled.
func noncopyable(_ b: borrowing NoncopyableBase?) { // expected-error {{'b' is borrowed and cannot be consumed}}
  f(b?.view) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{consumed here}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
  // expected-note @-4 {{it depends on the lifetime of argument 'b'}}
}
