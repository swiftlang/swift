// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain

// REQUIRES: swift_feature_CustomAvailability

func alwaysAvailable() { }

@available(EnabledDomain)
func availableInEnabledDomain() { }

@available(EnabledDomain, unavailable)
func unavailableInEnabledDomain() { }

@available(DisabledDomain, unavailable)
func unavailableInDisabledDomain() { } // expected-note 4 {{'unavailableInDisabledDomain()' has been explicitly marked unavailable here}}

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(DynamicDomain, deprecated, message: "Use something else")
func deprecatedInDynamicDomain() { }

@available(UnknownDomain) // expected-warning {{unrecognized platform name 'UnknownDomain'}}
func availableInUnknownDomain() { }

func testDeployment() {
  alwaysAvailable()
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}

@available(EnabledDomain)
func testEnabledDomainAvailable() {
  availableInEnabledDomain() // OK
  if #available(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant
  // expected-error@-1 {{condition required for target platform}}
  if #unavailable(EnabledDomain) {}

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}


@available(EnabledDomain, unavailable)
func testEnabledDomainUnavailable() {
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  if #available(EnabledDomain) {} // FIXME: [availability] Maybe diagnose?
  // expected-error@-1 {{condition required for target platform}}
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}

@available(DisabledDomain, unavailable)
func testDisabledDomainUnavailable() {
  unavailableInDisabledDomain() // OK
}

@available(*, unavailable)
func testUniversallyUnavailable() {
  alwaysAvailable()
  // FIXME: [availability] Diagnostic consistency: potentially unavailable declaration shouldn't be diagnosed
  // in contexts that are unavailable to broader domains
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  unavailableInDisabledDomain()
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}

@available(EnabledDomain)
struct EnabledDomainAvailable {
  @available(DynamicDomain)
  func dynamicDomainAvailableMethod() {
    availableInEnabledDomain() // OK
    availableInDynamicDomain() // OK

    alwaysAvailable()
    unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  }
}
