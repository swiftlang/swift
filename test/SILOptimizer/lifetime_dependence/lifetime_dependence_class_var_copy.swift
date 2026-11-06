// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// rdar://191300779 (https://github.com/swiftlang/swift/issues/93712)
//
// A borrow dependence on a class `var` property depends on the copy loaded within the property's access scope, not on
// the access itself. The access may be nested in the borrow scope of a temporary reference that has already ended.

struct Thing: ~Escapable {
  var p: Int
  @_lifetime(immortal) init() { p = 0 }
}

final class C {
  var next: C? = nil
  var span: Thing {
    @_lifetime(borrow self) get { _overrideLifetime(Thing(), borrowing: self) }
  }
}

final class D {
  var c: C = C()
  let lc: C = C()
}

final class E {
  var d: D = D()
  let ld: D = D()
}

func use(_ t: borrowing Thing) {}
func f(_ s: Optional<Thing>) {}
func makeD() -> D { D() }

// --- Accepted: LifetimeDependenceScopeFixup extends the loaded copy over the dependent uses. ---

func argVar(_ d: D) {
  f(d.c.span)
}

func argVarLocal(_ d: D) {
  let t = d.c.span
  use(t)
}

func letThenVar(_ e: E) {
  use(e.ld.c.span)
}

func letThenVarLocal(_ e: E) {
  let t = e.ld.c.span
  use(t)
}

func varThenVar(_ e: E) {
  use(e.d.c.span)
}

func callResultThenVar() {
  use(makeD().c.span)
}

func localVarThenVar(_ d: D) {
  var x = d
  x = D()
  use(x.c.span)
}

// --- Rejected: the copy cannot be extended past the merge of the optional chain. ---

func optionalThenVar(_ d: D?) {
  f(d?.c.span) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'c'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func optionalChain(_ x: C?) {
  f(x?.next?.span) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'next'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}

func classPropChain(_ h: C) {
  f(h.next?.next?.span) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'next'}}
  // expected-note @-2 {{this use of the lifetime-dependent value is out of scope}}
}
