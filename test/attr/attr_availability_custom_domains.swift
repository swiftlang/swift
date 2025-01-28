// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-enabled-availability-domain RedefinedDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain \
// RUN:  -define-disabled-availability-domain RedefinedDomain

// REQUIRES: swift_feature_CustomAvailability

@available(EnabledDomain) // expected-warning {{unknown platform 'EnabledDomain' for attribute 'available'}}
func availableInEnabledDomain() { }

@available(DisabledDomain, unavailable) // expected-warning {{unknown platform 'DisabledDomain' for attribute 'available'}}
func availableInDisabledDomain() { }

@available(RedefinedDomain, deprecated, message: "Use something else") // expected-warning {{unknown platform 'RedefinedDomain' for attribute 'available'}}
func availableInRedefinedDomain() { }

@available(DynamicDomain) // expected-warning {{unknown platform 'DynamicDomain' for attribute 'available'}}
func availableInDynamicDomain() { }

@available(UnknownDomain) // expected-warning {{unknown platform 'UnknownDomain' for attribute 'available'}}
func availableInUnknownDomain() { }

func test() {
  availableInEnabledDomain()
  availableInDisabledDomain()
  availableInRedefinedDomain()
  availableInDynamicDomain()
  availableInUnknownDomain()
}
