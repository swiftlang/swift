// RUN: %target-typecheck-verify-swift

// Protocol metatype extensions are reserved for COM interop: only a protocol
// marked @com may carry one, and there is no user-facing feature flag that
// unlocks them for arbitrary protocols. A metatype extension written on a
// non-@com *protocol* is rejected as reserved for COM interop.

protocol P {}

extension P.Protocol { // expected-error {{protocol metatype extensions are reserved for COM interop}}
  var value: Int { 42 }
  func greet() -> String { "hello" }
}

// --- Refinement of a plain protocol is still not a COM interface.
protocol Q: P {}
extension Q.Protocol {} // expected-error {{protocol metatype extensions are reserved for COM interop}}

// --- `.Protocol` on a non-protocol type is not a protocol metatype at all, so
//     it is rejected as an invalid metatype spelling before the COM-only rule
//     is reached.
struct S {}
extension S.Protocol {} // expected-error {{cannot use 'Protocol' with non-protocol type 'S'}}

class C {}
extension C.Protocol {} // expected-error {{cannot use 'Protocol' with non-protocol type 'C'}}
