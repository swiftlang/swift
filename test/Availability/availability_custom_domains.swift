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

@available(EnabledDomain)
@available(EnabledDomain)
func availableInEnabledDomainTwice() { }

@available(EnabledDomain)
@available(EnabledDomain, unavailable)
func availableAndUnavailableInEnabledDomain() { } // expected-note {{'availableAndUnavailableInEnabledDomain()' has been explicitly marked unavailable here}}

func testDeployment() { // expected-note 3 {{add '@available' attribute to enclosing global function}}
  alwaysAvailable()
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  availableInUnknownDomain()
  availableInEnabledDomainTwice() // expected-error {{'availableInEnabledDomainTwice()' is only available in EnabledDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  availableAndUnavailableInEnabledDomain() // expected-error {{'availableAndUnavailableInEnabledDomain()' is unavailable}}
}

func testIfAvailable(_ truthy: Bool) { // expected-note 9 {{add '@available' attribute to enclosing global function}}
  if #available(EnabledDomain) { // expected-note {{enclosing scope here}}
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
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
      // expected-note@-1 {{add 'if #available' version check}}
      unavailableInDynamicDomain()
    }

    if #available(EnabledDomain) {} // expected-warning {{unnecessary check for 'EnabledDomain'; enclosing scope ensures guard will always be true}}

    if #unavailable(EnabledDomain) { // FIXME: [availability] Diagnose as unreachable
      // Unreachable.
      availableInEnabledDomain()
      unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
      availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
      // expected-note@-1 {{add 'if #available' version check}}
      unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
    }
  } else {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInEnabledDomain()
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
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
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInDynamicDomain() // expected-error {{'unavailableInDynamicDomain()' is unavailable}}
  }

  if #available(EnabledDomain), truthy {
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  } else {
    // In this branch, the state of EnabledDomain remains unknown since
    // execution will reach here if "truthy" is false.
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  }

  if #unavailable(EnabledDomain) {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
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

func testWhileAvailable() { // expected-note {{add '@available' attribute to enclosing global function}}
  while #available(EnabledDomain) { // expected-note {{enclosing scope here}}
    availableInEnabledDomain()
    unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}

    if #available(EnabledDomain) {} // expected-warning {{unnecessary check for 'EnabledDomain'; enclosing scope ensures guard will always be true}}
    if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable
  }

  while #unavailable(EnabledDomain) {
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInEnabledDomain()

    if #available(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable
    if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant
  }
}

func testGuardAvailable() { // expected-note 3 {{add '@available' attribute to enclosing global function}}
  guard #available(EnabledDomain) else { // expected-note {{enclosing scope here}}
    availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
    // expected-note@-1 {{add 'if #available' version check}}
    unavailableInEnabledDomain()
    availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
    // expected-note@-1 {{add 'if #available' version check}}

    return
  }

  availableInEnabledDomain()
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  // expected-note@-1 {{add 'if #available' version check}}

  if #available(EnabledDomain) {} // expected-warning {{unnecessary check for 'EnabledDomain'; enclosing scope ensures guard will always be true}}
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable
}

@available(EnabledDomain)
func testEnabledDomainAvailable() { // expected-note {{add '@available' attribute to enclosing global function}} expected-note {{enclosing scope here}}
  availableInEnabledDomain()
  unavailableInEnabledDomain() // expected-error {{'unavailableInEnabledDomain()' is unavailable}}

  if #available(EnabledDomain) {} // expected-warning {{unnecessary check for 'EnabledDomain'; enclosing scope ensures guard will always be true}}
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  availableInUnknownDomain()
}

@available(EnabledDomain, unavailable)
func testEnabledDomainUnavailable() { // expected-note {{add '@available' attribute to enclosing global function}}
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  unavailableInEnabledDomain()

  if #available(EnabledDomain) {} // FIXME: [availability] Diagnose as unreachable
  if #unavailable(EnabledDomain) {} // FIXME: [availability] Diagnose as redundant

  alwaysAvailable()
  unavailableInDisabledDomain() // expected-error {{'unavailableInDisabledDomain()' is unavailable}}
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  availableInUnknownDomain()
}

@available(*, unavailable)
func testUniversallyUnavailable() {
  alwaysAvailable()
  // FIXME: [availability] Diagnostic consistency: potentially unavailable declaration shouldn't be diagnosed
  // in contexts that are unavailable to broader domains
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
  unavailableInDisabledDomain()
  deprecatedInDynamicDomain() // expected-warning {{'deprecatedInDynamicDomain()' is deprecated: Use something else}}
  availableInDynamicDomain() // expected-error {{'availableInDynamicDomain()' is only available in DynamicDomain}}
  // expected-note@-1 {{add 'if #available' version check}}
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

func testFixIts() {
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}{{1-1=@available(EnabledDomain)\n}}
  availableInEnabledDomain() // expected-error {{'availableInEnabledDomain()' is only available in EnabledDomain}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-29=if #available(EnabledDomain) {\n      availableInEnabledDomain()\n  \} else {\n      // Fallback\n  \}}}
}

struct Container { }

@available(EnabledDomain)
extension Container {
  @available(EnabledDomain)
  func redundantlyAvailableInEnabledDomain() { }

  @available(EnabledDomain, unavailable)
  func unavailableInEnabledDomain() { }
}

protocol OpaqueReturnType { }

@available(EnabledDomain)
struct AvailableOpaqueReturnType: OpaqueReturnType { }

@available(EnabledDomain, unavailable)
struct UnavailableOpaqueReturnType: OpaqueReturnType { }

func testOpaqueReturnType() -> some OpaqueReturnType {
  if #available(EnabledDomain) { // expected-error {{opaque return type cannot depend on EnabledDomain availability}}
    return AvailableOpaqueReturnType()
  } else {
    return UnavailableOpaqueReturnType()
  }
}

protocol HasRequirementInEnabledDomain {
  func alwaysAvailableRequirement()
  // expected-note@-1 3 {{protocol requirement here}}
  // expected-note@-2 {{requirement 'alwaysAvailableRequirement()' declared here}}

  @available(EnabledDomain)
  func availableInEnabledDomain()
  // expected-note@-1 2 {{protocol requirement here}}
  // expected-note@-2 {{requirement 'availableInEnabledDomain()' declared here}}
}

protocol HasAssocTypeRequirementInEnabledDomain {
  associatedtype A // expected-note {{requirement 'A' declared here}}

  @available(EnabledDomain)
  associatedtype B // expected-note {{requirement 'B' declared here}}
}

protocol HasRequirementUnavailableInEnabledDomain {
  @available(EnabledDomain, unavailable) // expected-error {{protocol members can only be marked unavailable in an '@objc' protocol}}
  func unavailableInEnabledDomain()
}

struct ConformsToHasRequirementsInEnabledDomain: HasRequirementInEnabledDomain {
  func alwaysAvailableRequirement() { }
  func availableInEnabledDomain() { }
}

@available(EnabledDomain)
struct ConformsToHasRequirementsInEnabledDomain1: HasRequirementInEnabledDomain {
  func alwaysAvailableRequirement() { }
  func availableInEnabledDomain() { }
}

@available(EnabledDomain, unavailable)
struct ConformsToHasRequirementsInEnabledDomain2: HasRequirementInEnabledDomain {
  func alwaysAvailableRequirement() { }
  func availableInEnabledDomain() { }
}

struct ConformsToHasRequirementsInEnabledDomain3: HasRequirementInEnabledDomain {
  @available(EnabledDomain)
  func alwaysAvailableRequirement() { } // expected-error {{protocol 'HasRequirementInEnabledDomain' requirement 'alwaysAvailableRequirement()' cannot be satisfied by instance method that is only available in EnabledDomain}}
  @available(EnabledDomain)
  func availableInEnabledDomain() { }
}

struct ConformsToHasRequirementsInEnabledDomain4: HasRequirementInEnabledDomain { // expected-error {{type 'ConformsToHasRequirementsInEnabledDomain4' does not conform to protocol 'HasRequirementInEnabledDomain'}}
  @available(EnabledDomain, unavailable)
  func alwaysAvailableRequirement() { } // expected-error {{unavailable instance method 'alwaysAvailableRequirement()' was used to satisfy a requirement of protocol 'HasRequirementInEnabledDomain'}}
  @available(EnabledDomain, unavailable)
  func availableInEnabledDomain() { } // expected-error {{unavailable instance method 'availableInEnabledDomain()' was used to satisfy a requirement of protocol 'HasRequirementInEnabledDomain'}}
}

struct ConformsToHasRequirementsInEnabledDomain5: HasRequirementInEnabledDomain {
  @available(DisabledDomain)
  func alwaysAvailableRequirement() { } // expected-error {{protocol 'HasRequirementInEnabledDomain' requirement 'alwaysAvailableRequirement()' cannot be satisfied by instance method that is only available in DisabledDomain}}
  @available(DisabledDomain)
  func availableInEnabledDomain() { } // expected-error {{protocol 'HasRequirementInEnabledDomain' requirement 'availableInEnabledDomain()' cannot be satisfied by instance method that is only available in DisabledDomain}}
}

struct ConformsToHasRequirementsInEnabledDomain6: HasRequirementInEnabledDomain {
  @available(EnabledDomain)
  @available(DisabledDomain)
  func alwaysAvailableRequirement() { } // expected-error {{protocol 'HasRequirementInEnabledDomain' requirement 'alwaysAvailableRequirement()' cannot be satisfied by instance method that is only available in DisabledDomain}}
  @available(EnabledDomain)
  @available(DisabledDomain)
  func availableInEnabledDomain() { } // expected-error {{protocol 'HasRequirementInEnabledDomain' requirement 'availableInEnabledDomain()' cannot be satisfied by instance method that is only available in DisabledDomain}}
}

struct ConformsToHasAssocTypeRequirementInEnabledDomain: HasAssocTypeRequirementInEnabledDomain {
  struct A { }
  struct B { }
}

struct ConformsToHasAssocTypeRequirementInEnabledDomain1: HasAssocTypeRequirementInEnabledDomain { // expected-error {{type 'ConformsToHasAssocTypeRequirementInEnabledDomain1' does not conform to protocol 'HasAssocTypeRequirementInEnabledDomain'}}
  @available(EnabledDomain)
  struct A { } // expected-error {{protocol 'HasAssocTypeRequirementInEnabledDomain' requirement 'A' cannot be satisfied by struct that is only available in EnabledDomain}}

  @available(EnabledDomain)
  struct B { }
}

struct ConformsToHasAssocTypeRequirementInEnabledDomain2: HasAssocTypeRequirementInEnabledDomain { // expected-error {{type 'ConformsToHasAssocTypeRequirementInEnabledDomain2' does not conform to protocol 'HasAssocTypeRequirementInEnabledDomain'}}
  @available(EnabledDomain, unavailable)
  struct A { } // expected-error {{unavailable struct 'A' was used to satisfy a requirement of protocol 'HasAssocTypeRequirementInEnabledDomain'}}

  @available(EnabledDomain, unavailable)
  struct B { } // expected-error {{unavailable struct 'B' was used to satisfy a requirement of protocol 'HasAssocTypeRequirementInEnabledDomain'}}
}

struct ConformsToHasAssocTypeRequirementInEnabledDomain3: HasAssocTypeRequirementInEnabledDomain {  // expected-error {{type 'ConformsToHasAssocTypeRequirementInEnabledDomain3' does not conform to protocol 'HasAssocTypeRequirementInEnabledDomain'}}
  @available(DisabledDomain)
  struct A { } // expected-error {{protocol 'HasAssocTypeRequirementInEnabledDomain' requirement 'A' cannot be satisfied by struct that is only available in DisabledDomain}}

  @available(DisabledDomain)
  struct B { } // expected-error {{protocol 'HasAssocTypeRequirementInEnabledDomain' requirement 'B' cannot be satisfied by struct that is only available in DisabledDomain}}
}
