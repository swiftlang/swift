// RUN: %target-typecheck-verify-swift \
// RUN:  -define-enabled-availability-domain SomeDomain

@available(SomeDomain, unavailable) // expected-error {{'SomeDomain' requires -enable-experimental-feature CustomAvailability}}
func availableInSomeDomain() { }
