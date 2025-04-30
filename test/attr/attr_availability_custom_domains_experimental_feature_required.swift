// RUN: %target-typecheck-verify-swift \
// RUN:  -define-enabled-availability-domain SomeDomain

@available(SomeDomain, unavailable) // expected-error {{SomeDomain requires '-enable-experimental-feature CustomAvailability'}}
func availableInSomeDomain() { }

if #available(SomeDomain) {} // expected-error {{SomeDomain requires '-enable-experimental-feature CustomAvailability'}}
// expected-error@-1 {{condition required for target platform}}
if #unavailable(SomeDomain) {} // expected-error {{SomeDomain requires '-enable-experimental-feature CustomAvailability'}}
