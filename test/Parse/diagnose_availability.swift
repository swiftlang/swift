// RUN: %target-typecheck-verify-swift 

// https://github.com/apple/swift/issues/46814
// Misleading/wrong error message for malformed '@available'

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


// https://github.com/apple/swift/issues/51114
// Missing/wrong warning message for '*' or 'swift' platform.

@available(*, deprecated: 4.2)
// expected-warning@-1 {{unexpected version number for *}}
func allPlatformsDeprecatedVersion() {}

@available(*, deprecated, obsoleted: 4.2)
// expected-warning@-1 {{unexpected version number for *}}
func allPlatformsDeprecatedAndObsoleted() {}

@available(*, introduced: 4.0, deprecated: 4.1, obsoleted: 4.2)
// expected-warning@-1 {{unexpected version number for *}}
func allPlatformsDeprecatedAndObsoleted2() {}

@available(swift, unavailable)
// expected-warning@-1 {{'unavailable' cannot be used in '@available' attribute for Swift}}
func swiftUnavailable() {}

@available(swift, unavailable, introduced: 4.2)
// expected-warning@-1 {{'unavailable' cannot be used in '@available' attribute for Swift}}
func swiftUnavailableIntroduced() {}

@available(swift, deprecated)
// expected-warning@-1 {{expected version number with 'deprecated' in '@available' attribute for Swift}}
func swiftDeprecated() {}

@available(swift, deprecated, obsoleted: 4.2)
// expected-warning@-1 {{expected version number with 'deprecated' in '@available' attribute for Swift}}
func swiftDeprecatedObsoleted() {}

@available(swift, message: "missing valid option")
// expected-warning@-1 {{expected 'introduced', 'deprecated', or 'obsoleted' in '@available' attribute for Swift}}
func swiftMessage() {}

@available(swift 5, deprecated: 6)
// expected-error@-1 {{'deprecated' can't be combined with shorthand specification 'swift 5'}}
// expected-note@-2 {{did you mean to specify an introduction version?}} {{17-17=, introduced:}}
// expected-error@-3 {{expected declaration}}
func swiftShorthandFollowedByDeprecated() {}

@available(*, unavailable, message: "\("message")")
// expected-error@-1{{'message' cannot be an interpolated string literal}}
func interpolatedMessage() {}

@available(*, unavailable, message: """
  foobar message.
  """)
func multilineMessage() {}
multilineMessage()
// expected-error@-1{{'multilineMessage()' is unavailable: foobar message.}}
// expected-note@-3{{'multilineMessage()' has been explicitly marked unavailable here}}

@available(*, unavailable, message: " ")
func emptyMessage() {}
emptyMessage()
// expected-error@-1{{'emptyMessage()' is unavailable:  }}
// expected-note@-3{{'emptyMessage()' has been explicitly marked unavailable here}}

// expected-error@+1{{'message' cannot be an extended escaping string literal}}
@available(*, unavailable, message: #"""
  foobar message.
  """#)
func extendedEscapedMultilineMessage() {}

// expected-error@+1{{'renamed' cannot be an extended escaping string literal}}
@available(*, unavailable, renamed: #"available()"#)
func extendedEscapedRenamed() {}
