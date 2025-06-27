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
func unavailableInEnabledDomain() { } // expected-note * {{'unavailableInEnabledDomain()' has been explicitly marked unavailable here}}

@available(DisabledDomain, unavailable)
func unavailableInDisabledDomain() { } // expected-note * {{'unavailableInDisabledDomain()' has been explicitly marked unavailable here}}

@available(DynamicDomain)
func availableInDynamicDomain() { }

@available(DynamicDomain, deprecated, message: "Use something else")
func deprecatedInDynamicDomain() { }

@available(DynamicDomain, unavailable)
func unavailableInDynamicDomain() { } // expected-note * {{'unavailableInDynamicDomain()' has been explicitly marked unavailable here}}

@available(UnknownDomain) // expected-error {{unrecognized platform name 'UnknownDomain'}}
func availableInUnknownDomain() { }

func testDeployment() {
  alwaysAvailable()
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}

func testIfAvailable(_ truthy: Bool) {
  if #available(EnabledDomain) {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}

    if #available(DynamicDomain) {
      availableInEnabledDomain()
      unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
      availableInDynamicDomain()
      unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
    } else {
      availableInEnabledDomain()
      unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
      availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
      unavailableInDynamicDomain()
    }

    if #unavailable(EnabledDomain) {
      // Unreachable.
      availableInEnabledDomain()
      unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
      availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
      unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
    }
  } else {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain()
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  }

  if #available(EnabledDomain), #available(DynamicDomain) {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
    availableInDynamicDomain()
    unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  } else {
    // In this branch, we only know that one of the domains is unavailable,
    // but we don't know which.
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  }

  if #available(EnabledDomain), truthy {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  } else {
    // In this branch, the state of EnabledDomain remains unknown since
    // execution will reach here if "truthy" is false.
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  }

  if #unavailable(EnabledDomain) {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain()
  } else {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  }

  // FIXME: [availability] Support mixed #available and #unavailable.
  if #unavailable(EnabledDomain), #available(DynamicDomain) {
    // expected-error@-1 {{#available and #unavailable cannot be in the same statement}}
  }
}

func testWhileAvailable() {
  while #available(EnabledDomain) {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  }

  while #unavailable(EnabledDomain) {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain()
  }
}

func testGuardAvailable() {
  guard #available(EnabledDomain) else {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    unavailableInEnabledDomain()
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}

    return
  }

  availableInEnabledDomain()
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
}

@available(EnabledDomain)
func testEnabledDomainAvailable() {
  availableInEnabledDomain()
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}

  if #available(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
}

@available(EnabledDomain, unavailable)
func testEnabledDomainUnavailable() {
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  unavailableInEnabledDomain()

  if #available(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  availableInUnknownDomain()
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

  if #available(EnabledDomain) {} // FIXME: [availability] Diagnose?
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose?
}

@available(EnabledDomain)
struct EnabledDomainAvailable {
  @available(DynamicDomain)
  func dynamicDomainAvailableMethod() {
    availableInEnabledDomain()
    availableInDynamicDomain()

    alwaysAvailable()
    unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  }
}
