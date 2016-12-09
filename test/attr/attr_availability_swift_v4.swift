// RUN: %target-typecheck-verify-swift -swift-version 4 %s

@available(swift 3)
func swiftShortThree() {}

@available(swift 3.0)
func swiftShortThreePointOh() {}

@available(swift, introduced: 3.0)
func swiftThreePointOh() {}

@available(swift, introduced: 3.0, obsoleted: 4.0)
func swiftThreePointOhOnly() {} // expected-note {{was obsoleted in Swift 4.0}}

@available(swift, deprecated: 3.0)
func swiftDeprecatedThreePointOh() {}

@available(swift, obsoleted: 3.0)
func swiftObsoletedThreePointOh() {} // expected-note {{was obsoleted in Swift 3.0}}

@available(swift, introduced: 3.0, obsoleted: 4.0)
class SwiftThreePointOhOnly {} // expected-note {{was obsoleted in Swift 4.0}}

@available(swift, introduced: 3, obsoleted: 4, message: "uses abc")
class SwiftThreeOnlyWithMessage {} // expected-note {{was obsoleted in Swift 4}}


@available(swift 4)
func swiftShortFour() {}

@available(swift 4.0)
func swiftShortFourPointOh() {}

@available(swift, introduced: 4)
func swiftFour() {}

@available(swift, introduced: 4.0)
func swiftFourPointOh() {}

@available(swift 4)
class SwiftShortFour {}


swiftShortThree()
swiftShortThreePointOh()
swiftThreePointOh()
swiftThreePointOhOnly() // expected-error {{is unavailable}}
swiftDeprecatedThreePointOh() // expected-warning {{is deprecated}}
swiftObsoletedThreePointOh() // expected-error {{is unavailable}}
let a : SwiftThreePointOhOnly // expected-error {{is unavailable}}
let b : SwiftThreeOnlyWithMessage // expected-error {{is unavailable: uses abc}}


swiftShortFour()
swiftShortFourPointOh()
swiftFour()
swiftFourPointOh()
let aa : SwiftShortFour
