// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-enabled-availability-domain RedefinedDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain \
// RUN:  -define-disabled-availability-domain RedefinedDomain

// REQUIRES: swift_feature_CustomAvailability

// FIXME: [availability] Test custom domains in availability macros

func alwaysAvailable() { }

@available(EnabledDomain)
func availableInEnabledDomain() { }

@available(EnabledDomain, *) // expected-error {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
// expected-error@-1 {{expected declaration}}
func availableInEnabledDomainWithWildcard() { }

@available(EnabledDomain, introduced: 1.0) // expected-error {{unexpected version number for EnabledDomain}}
func introducedInEnabledDomain() { }

@available(EnabledDomain 1.0) // expected-error {{unexpected version number for EnabledDomain}}
func introducedInEnabledDomainShort() { }

@available(EnabledDomain 1.0, *) // expected-error {{unexpected version number for EnabledDomain}}
func introducedInEnabledDomainShortWithWildcard() { }

@available(macOS 10.10, EnabledDomain, *) // expected-error {{EnabledDomain availability must be specified alone}}
func introducedInMacOSAndAvailableInEnabledDomain() { }

@available(EnabledDomain, macOS 10.10, *) // expected-error {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
// expected-error@-1 {{expected declaration}}
func availableInEnabledDomainAndIntroducedInMacOS() { }

@available(EnabledDomain, DisabledDomain) // expected-error {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
func availableInMultipleCustomDomainsShort() { }

@available(EnabledDomain, DisabledDomain, *) // expected-error {{expected 'available' option such as 'unavailable', 'introduced', 'deprecated', 'obsoleted', 'message', or 'renamed'}}
// expected-error@-1 {{expected declaration}}
func availableInMultipleCustomDomainsShortWithWildcard() { }

@available(EnabledDomain, deprecated: 1.0) // expected-error {{unexpected version number for EnabledDomain}}
func deprecatedInEnabledDomain() { }

@available(EnabledDomain, obsoleted: 1.0) // expected-error {{unexpected version number for EnabledDomain}}
func obsoletedInEnabledDomain() { }

@available(DisabledDomain, unavailable)
func unavailableInDisabledDomain() { }

@available(RedefinedDomain, deprecated, message: "Use something else")
func deprecatedInRedefinedDomain() { }

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(UnknownDomain) // expected-error {{unrecognized platform name 'UnknownDomain'}}
func availableInUnknownDomain() { }

@available(EnabledDomain)
@available(EnabledDomain)
func availableInEnabledDomainTwice() { }

@available(EnabledDomain)
@available(EnabledDomain, unavailable)
func availableAndUnavailableInEnabledDomain() { }
