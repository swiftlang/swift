// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix wae- -strict-concurrency=complete -warnings-as-errors %s
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix nowae- -strict-concurrency=complete %s

// REQUIRES: concurrency

// Preconcurrency downgrades change the effective behavior of diagnostics without
// wrapping them. Check that WAE applies to errors which were downgraded to
// warnings.

class NS {} // expected-note {{class 'NS' does not conform to the 'Sendable' protocol}}

@preconcurrency func takeSendable<T: Sendable>(_ s: T) {}

takeSendable(NS())
// expected-wae-error@-1 {{type 'NS' does not conform to the 'Sendable' protocol}}
// expected-nowae-warning@-2 {{type 'NS' does not conform to the 'Sendable' protocol}}
