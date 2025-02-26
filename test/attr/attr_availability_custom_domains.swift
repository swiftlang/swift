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

@available(EnabledDomain, introduced: 1.0) // expected-warning {{unexpected version number in '@available' attribute for 'EnabledDomain'}}
func introducedInEnabledDomain() { }

@available(EnabledDomain, deprecated: 1.0) // expected-warning {{unexpected version number in '@available' attribute for 'EnabledDomain'}}
func deprecatedInEnabledDomain() { }

@available(EnabledDomain, obsoleted: 1.0) // expected-warning {{unexpected version number in '@available' attribute for 'EnabledDomain'}}
func obsoletedInEnabledDomain() { }

@available(DisabledDomain, unavailable)
func unavailableInDisabledDomain() { } // expected-note {{'unavailableInDisabledDomain()' has been explicitly marked unavailable here}}

@available(RedefinedDomain, deprecated, message: "Use something else")
func deprecatedInRedefinedDomain() { }

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(UnknownDomain) // expected-warning {{unrecognized platform name 'UnknownDomain'}}
func availableInUnknownDomain() { }

func test() {
  availableInEnabledDomain() // FIXME: [availability] should be diagnosed
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInRedefinedDomain() // expected-warning {{'deprecatedInRedefinedDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // FIXME: [availability] should be diagnosed
  availableInUnknownDomain() // Ok
}
