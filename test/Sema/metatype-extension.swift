// RUN: %target-typecheck-verify-swift

// Protocol metatype extensions are reserved for COM interop: only a protocol
// marked @com may carry one, and there is no user-facing feature flag that
// unlocks them for arbitrary protocols. Every metatype extension written on a
// non-@com type is rejected before any of the other metatype-extension
// constraints are considered.

protocol P {}

extension P.Protocol { // expected-error {{protocol metatype extensions are reserved for COM interop}}
  var value: Int { 42 }
  func greet() -> String { "hello" }
}

// --- Refinement of a plain protocol is still not a COM interface.
protocol Q: P {}
extension Q.Protocol {} // expected-error {{protocol metatype extensions are reserved for COM interop}}

// --- Metatype extension on non-protocol types is rejected the same way; the
//     COM-only check runs first.
struct S {}
extension S.Protocol {} // expected-error {{protocol metatype extensions are reserved for COM interop}}

class C {}
extension C.Protocol {} // expected-error {{protocol metatype extensions are reserved for COM interop}}
