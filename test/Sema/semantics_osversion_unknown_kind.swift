// RUN: %target-typecheck-verify-swift

@_semantics("availability.osversion(bogus)") // expected-error {{unknown OS version query kind 'bogus' in '@_semantics' attribute}}
public func badQuery() {}
