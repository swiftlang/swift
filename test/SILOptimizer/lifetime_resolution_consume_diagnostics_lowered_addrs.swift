// Use-after-consume diagnostics from LifetimeResolution when address-only types are lowered to addresses.
// For the bulk of these diagnostics, see sibling file without `lowered_addrs` in its name.

// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -disable-sil-opaque-values -verify %s
// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -enable-sil-opaque-values -verify %s

class C {}
struct NC: ~Copyable { var c: C }
func useNC(_ x: borrowing NC) {}
func useGeneric<T: ~Copyable>(_ t: borrowing T) {}

// The binding consumes `t`, not the call that borrows the binding.
func consumedIntoGenericBinding<T: ~Copyable>(_ t: consuming T) {
  let u = t // expected-note {{consumed here}}
  useGeneric(u)
  useGeneric(t) // expected-error {{'t' used after consume}}
}
