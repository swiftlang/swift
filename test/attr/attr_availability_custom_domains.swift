// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-enabled-availability-domain RedefinedDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain \
// RUN:  -define-disabled-availability-domain RedefinedDomain

// REQUIRES: swift_feature_CustomAvailability

@available(EnabledDomain)
func availableInEnabledDomain() { }

@available(DisabledDomain, unavailable)
func unavailableInDisabledDomain() { } // expected-note {{'unavailableInDisabledDomain()' has been explicitly marked unavailable here}}

@available(RedefinedDomain, deprecated, message: "Use something else")
func deprecatedInRedefinedDomain() { }

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(UnknownDomain) // expected-warning {{unknown platform 'UnknownDomain' for attribute 'available'}}
func availableInUnknownDomain() { }

func test() {
  availableInEnabledDomain() // FIXME: [availability] should be diagnosed
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInRedefinedDomain() // expected-warning {{'deprecatedInRedefinedDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // FIXME: [availability] should be diagnosed
  availableInUnknownDomain() // Ok
}
