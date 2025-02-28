// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-enabled-availability-domain RedefinedDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain \
// RUN:  -define-disabled-availability-domain RedefinedDomain

// REQUIRES: swift_feature_CustomAvailability

// FIXME: [availability] Test custom domains in availability macros

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
func unavailableInDisabledDomain() { } // expected-note {{'unavailableInDisabledDomain()' has been explicitly marked unavailable here}}

@available(RedefinedDomain, deprecated, message: "Use something else")
func deprecatedInRedefinedDomain() { }

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(UnknownDomain) // expected-warning {{unrecognized platform name 'UnknownDomain'}}
func availableInUnknownDomain() { }

func testQuerySyntax() {
  if #available(EnabledDomain) {}
  // expected-error@-1 {{condition required for target platform}}
  if #available(RedefinedDomain) {}
  // expected-error@-1 {{condition required for target platform}}
  if #available(DisabledDomain) {}
  // expected-error@-1 {{condition required for target platform}}
  if #available(DynamicDomain) {}
  // expected-error@-1 {{condition required for target platform}}
  if #available(UnknownDomain) {} // expected-warning {{unrecognized platform name 'UnknownDomain'}}
  // expected-error@-1 {{condition required for target platform}}

  if #unavailable(EnabledDomain) {}
  if #unavailable(RedefinedDomain) {}
  if #unavailable(DisabledDomain) {}
  if #unavailable(DynamicDomain) {}
  if #unavailable(UnknownDomain) {} // expected-warning {{unrecognized platform name 'UnknownDomain'}}

  if #available(EnabledDomain 1.0) {} // expected-error {{unexpected version number for EnabledDomain}}
  // expected-error@-1 {{condition required for target platform}}
  if #available(EnabledDomain, DisabledDomain) {} // expected-error {{EnabledDomain availability must be specified alone}}
  // expected-error@-1 {{condition required for target platform}}
}

func testAvailability() {
  availableInEnabledDomain() // FIXME: [availability] should be diagnosed
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInRedefinedDomain() // expected-warning {{'deprecatedInRedefinedDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // FIXME: [availability] should be diagnosed
  availableInUnknownDomain() // Ok
}
