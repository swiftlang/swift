// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/../Inputs/case_iterable_other.swift -verify-additional-prefix unsupported- -verify-ignore-unknown

extension FromOtherFile: CaseIterable {} // expected-error {{extension outside of file declaring enum 'FromOtherFile' prevents automatic synthesis of 'allCases' for protocol 'CaseIterable'}} expected-note {{add stubs for conformance}}

enum NotCaseIterableAssociatedValues: CaseIterable { // expected-error {{type 'NotCaseIterableAssociatedValues' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a(Int)
  case b
}

// FIXME: [availability] Deprecation should not block this conformance synthesis
enum NotCaseIterableUniversallyDeprecatedCase: CaseIterable { // expected-error {{type 'NotCaseIterableUniversallyDeprecatedCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(*, deprecated)
  case b
}

enum NotCaseIterableUniversallyUnavailableCase: CaseIterable { // expected-error {{type 'NotCaseIterableUniversallyUnavailableCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(*, unavailable)
  case b
}

enum NotCaseIterableSwiftIntroducedLaterCase: CaseIterable { // expected-error {{type 'NotCaseIterableSwiftIntroducedLaterCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(swift, introduced: 99)
  case b
}

enum NotCaseIterableSwiftIntroducedEarlierCase: CaseIterable { // expected-error {{type 'NotCaseIterableSwiftIntroducedEarlierCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(swift, introduced: 4)
  case b
}

enum NotCaseIterableSwiftObsoletedLaterCase: CaseIterable { // expected-error {{type 'NotCaseIterableSwiftObsoletedLaterCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(swift, obsoleted: 99)
  case b
}

enum NotCaseIterableSwiftObsoletedEarlierCase: CaseIterable { // expected-error {{type 'NotCaseIterableSwiftObsoletedEarlierCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(swift, obsoleted: 4)
  case b
}

enum NotCaseIterableMacOSUnavailableCase: CaseIterable { // expected-error {{type 'NotCaseIterableMacOSUnavailableCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(macOS, unavailable)
  case b
}

enum NotCaseIterableMacOSPotentiallyUnavailableCase: CaseIterable { // expected-error {{type 'NotCaseIterableMacOSPotentiallyUnavailableCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(macOS, introduced: 99)
  case b
}

enum NotCaseIterableMacOSObsoletedCase: CaseIterable { // expected-error {{type 'NotCaseIterableMacOSObsoletedCase' does not conform to protocol 'CaseIterable'}}
  // expected-note@-1 {{add stubs for conformance}}
  case a
  @available(macOS, obsoleted: 10.9)
  case b
}
