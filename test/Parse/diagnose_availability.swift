// RUN: %target-typecheck-verify-swift 

// SR-4231: Misleading/wrong error message for malformed @available

@available(OSX 10.6, *) // no error
func availableSince10_6() {}

@available(OSX, introduced: 10.0, deprecated: 10.12) // no error
func introducedFollowedByDeprecated() {}

@available(OSX 10.0, deprecated: 10.12)
// expected-error@-1 {{'deprecated' can't be combined with shorthand specification 'OSX 10.0'}}
// expected-note@-2 {{did you mean to specify an introduction version?}} {{15-15=, introduced:}}
// expected-error@-3 {{expected declaration}}
func shorthandFollowedByDeprecated() {}

@available(OSX 10.0, introduced: 10.12)
// expected-error@-1 {{'introduced' can't be combined with shorthand specification 'OSX 10.0'}}
// expected-error@-2 {{expected declaration}}
func shorthandFollowedByIntroduced() {}

@available(iOS 6.0, OSX 10.8, *) // no error
func availableOnMultiplePlatforms() {}

@available(iOS 6.0, OSX 10.0, deprecated: 10.12)
// expected-error@-1 {{'deprecated' can't be combined with shorthand specification 'OSX 10.0'}}
// expected-error@-2 {{expected declaration}}
func twoShorthandsFollowedByDeprecated() {}
