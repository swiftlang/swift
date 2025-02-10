// RUN: %target-typecheck-verify-swift \
// RUN:  -define-enabled-availability-domain SomeDomain

@available(SomeDomain, unavailable) // expected-error {{specifying 'SomeDomain' in '@available' attribute requires -enable-experimental-feature CustomAvailability}}
func availableInSomeDomain() { }
