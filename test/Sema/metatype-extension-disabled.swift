// RUN: %target-typecheck-verify-swift

// Protocol metatype extensions are an ad-hoc COM-interop construct. There is no
// user-facing feature flag that enables them on an arbitrary protocol; only a
// protocol marked @com may carry one. A bare metatype extension on a
// non-@com protocol is therefore always rejected.

protocol P {}

extension P.Protocol {} // expected-error {{protocol metatype extensions are reserved for COM interop}}
