// Use-after-consume diagnostics from LifetimeResolution when address-only types are lowered to addresses.
// For the bulk of these diagnostics, see sibling file without `lowered_addrs` in its name.

// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -disable-sil-opaque-values -verify %s
// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -enable-sil-opaque-values -verify %s

class C {}
struct NC: ~Copyable { var c: C }
func useNC(_ x: borrowing NC) {}
func useGeneric<T: ~Copyable>(_ t: borrowing T) {}

// The binding consumes `t`, not the call that borrows the binding.
func consumedIntoGenericBinding<T: ~Copyable>(_ t: consuming T) { // expected-note {{'t' declared here with noncopyable type 'T'}}
  let u = t // expected-note {{consumed here}}
  useGeneric(u)
  useGeneric(t) // expected-error {{'t' used after consume}}
}

// The indirect error result precedes the parameters.
struct TypedError: Error {}
func takeFirst<E: Error>(_ x: consuming NC, _ y: consuming NC, _ e: E.Type) throws(E) {} // expected-note {{consider making this parameter 'borrowing' if 'takeFirst' does not need ownership of it}} {{31-40=borrowing}}

func consumedByTypedThrowingCall() throws {
  let x = NC(c: C()) // expected-note {{'x' declared here with noncopyable type 'NC'}}
  try takeFirst(x, NC(c: C()), TypedError.self) // expected-note {{consumed here; 'takeFirst' takes ownership of this argument}}
  useNC(x) // expected-error {{'x' used after consume}}
}
