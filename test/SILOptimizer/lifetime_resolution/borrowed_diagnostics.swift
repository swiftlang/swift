// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify %s -o /dev/null

// Diagnostics from LifetimeResolution for borrowed values that are consumed.
// TODO: each of these should be a user-facing diagnostic rather than the missed-copy backstop.

class C {}
struct S { var c: C }
struct NC: ~Copyable {
  var c: C
  func f() {}
}
struct Inner: ~Copyable {}
struct Outer: ~Copyable { var a: Inner; var b: Inner }

func takeC(_ c: consuming C) {}
func useC(_ c: borrowing C) {}
func takeS(_ s: consuming S) {}
func takeNC(_ x: consuming NC) {}
func useNC(_ x: borrowing NC) {}
func takeInner(_ x: consuming Inner) {}
func mk() -> Outer { Outer(a: Inner(), b: Inner()) }

//===----------------------------------------------------------------------===//
// Borrowed noncopyable values
//===----------------------------------------------------------------------===//

func ncMethod(_ n: borrowing NC) { n.f() }
func ncUse(_ n: borrowing NC) { useNC(n) }
func ncField(_ n: borrowing NC) { takeC(n.c) }

func ncConsume(_ n: borrowing NC) {
  takeNC(n) // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func ncConsumeTwice(_ n: borrowing NC) {
  takeNC(n) // expected-error {{LifetimeResolution missed diagnosing a copy}}
  takeNC(n) // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func ncPartial(_ o: borrowing Outer) {
  takeInner(o.a) // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func ncLet(_ n: borrowing NC) {
  let x = n // expected-error {{LifetimeResolution missed diagnosing a copy}}
  useNC(x)
}

func ncReturn(_ n: borrowing NC) -> NC {
  return n // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func genericNC<T: ~Copyable>(_ t: borrowing T) -> T {
  return t // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

//===----------------------------------------------------------------------===//
// Borrowed Copyable values
//===----------------------------------------------------------------------===//

func cExplicit(_ c: borrowing C) { takeC(copy c) }
func cUse(_ c: borrowing C) { useC(c) }
func cField(_ s: borrowing S) { takeC(s.c) }

func cConsume(_ c: borrowing C) {
  takeC(c) // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func cLet(_ c: borrowing C) {
  let x = c // expected-error {{LifetimeResolution missed diagnosing a copy}}
  useC(x)
}

func cWhole(_ s: borrowing S) {
  takeS(s) // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func generic<T>(_ t: borrowing T) -> T {
  return t // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

//===----------------------------------------------------------------------===//
// Closure captures of borrowed values
//===----------------------------------------------------------------------===//

func callNonEscaping(_ f: () -> ()) { f() }

func captureNC(_ n: borrowing NC) {
  callNonEscaping { useNC(n) }
}

func captureAndConsumeNC(_ n: borrowing NC) {
  callNonEscaping {
    takeNC(n) // expected-error {{LifetimeResolution missed diagnosing a copy}}
  }
}

func captureAndConsumeC(_ c: borrowing C) {
  callNonEscaping {
    takeC(c) // expected-error {{LifetimeResolution missed diagnosing a copy}}
  }
}

func escapingCaptureNC(_ n: borrowing NC) -> () -> () {
  return { useNC(n) } // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func escapingCaptureC(_ c: borrowing C) -> () -> () {
  return { useC(c) } // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

//===----------------------------------------------------------------------===//
// Non-escaping closure captures converted to another function type
//===----------------------------------------------------------------------===//

func callRethrowing(_ f: () throws -> ()) rethrows { try f() }
class Base {}
class Derived: Base {}
func callContravariant(_ f: (Derived) -> ()) {}

func localFuncThrowsNC(_ n: borrowing NC) {
  func g() { useNC(n) }
  callRethrowing(g)
}

func localFuncThrowsC(_ c: borrowing C) {
  func g() { useC(c) }
  callRethrowing(g)
}

func localFuncNestedNC(_ n: borrowing NC) {
  callNonEscaping {
    func g() { useNC(n) }
    callRethrowing(g)
  }
}

func localFuncSubtypeC(_ c: borrowing C) {
  func g(_ x: Base) { useC(c) }
  callContravariant(g)
}

//===----------------------------------------------------------------------===//
// Closure captures passed to coroutine accessors
//===----------------------------------------------------------------------===//

struct Holder {
  var v = 0
  subscript(read f: () -> ()) -> Int { _read { f(); yield v } }
  subscript(mod f: () -> ()) -> Int {
    get { f(); return v }
    _modify { f(); yield &v }
  }
}

func readNC(_ n: borrowing NC, _ h: Holder) -> Int {
  return h[read: { useNC(n) }] // expected-error {{LifetimeResolution missed diagnosing a copy}}
}

func modifyC(_ c: borrowing C, _ h: inout Holder) {
  h[mod: { useC(c) }] = 3 // expected-error {{LifetimeResolution missed diagnosing a copy}}
}
