
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix none- %s
// RUN: %target-swift-frontend -typecheck -verify -sanitize=address -verify-additional-prefix address- %s
// RUN: %target-swift-frontend -typecheck -verify -sanitize=thread -verify-additional-prefix thread- %s
// RUN: %target-swift-frontend -typecheck -verify -sanitize=address -sanitize-coverage=func -verify-additional-prefix address- -verify-additional-prefix coverage- %s
// RUN: %target-swift-frontend -typecheck -verify -sanitize=fuzzer -verify-additional-prefix coverage- %s

// The condition reflects the module-wide build settings.

#if moduleInstrumentation(address)
#error("address") // expected-address-error {{address}}
#endif

#if moduleInstrumentation(thread)
#error("thread") // expected-thread-error {{thread}}
#endif

#if moduleInstrumentation(coverage)
#error("coverage") // expected-coverage-error {{coverage}}
#endif

#if !moduleInstrumentation(address) && !moduleInstrumentation(thread) && !moduleInstrumentation(coverage)
#error("none") // expected-none-error {{none}}
#endif

// Conditions may be combined with the usual operators.
#if moduleInstrumentation(address) || moduleInstrumentation(thread)
#error("address or thread")
// expected-address-error@-1 {{address or thread}}
// expected-thread-error@-2 {{address or thread}}
#endif

// The condition is not contextual: the attribute on the enclosing function has
// no effect on it.
@instrumentation(disable: address)
func disabled() {
#if moduleInstrumentation(address)
  #error("still address") // expected-address-error {{still address}}
#endif
}

// Diagnostics for malformed conditions.
#if moduleInstrumentation(bogus) // expected-error {{unexpected argument for the 'moduleInstrumentation' condition; expected 'address', 'thread', 'memtagStack', or 'coverage'}}
#endif

#if moduleInstrumentation(address, thread) // expected-error {{'moduleInstrumentation' requires a single unlabeled argument for the instrumentation kind}}
#endif

#if moduleInstrumentation() // expected-error {{'moduleInstrumentation' requires a single unlabeled argument for the instrumentation kind}}
#endif

#if moduleInstrumentation("address") // expected-error {{'moduleInstrumentation' requires a single unlabeled argument for the instrumentation kind}}
#endif
