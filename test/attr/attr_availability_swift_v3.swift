// RUN: %target-typecheck-verify-swift -swift-version 3 %s

@available(swift 3)
func swiftShortThree() {}

@available(swift 3.0)
func swiftShortThreePointOh() {}

@available(swift, introduced: 3.0)
func swiftThreePointOh() {}

@available(swift, introduced: 3.0, obsoleted: 4.0)
func swiftThreePointOhOnly() {}

@available(swift, deprecated: 3.0)
func swiftDeprecatedThreePointOh() {}

@available(swift, obsoleted: 3.0)
func swiftObsoletedThreePointOh() {} // expected-note {{was obsoleted in Swift 3.0}}

@available(swift, introduced: 3, obsoleted: 4.0)
class SwiftThreeOnly {}

@available(swift 4)
func swiftShortFour() {} // expected-note {{was introduced in Swift 4}}

@available(swift 4.0)
func swiftShortFourPointOh() {} // expected-note {{was introduced in Swift 4.0}}

@available(swift, introduced: 4)
func swiftFour() {} // expected-note {{was introduced in Swift 4}}

@available(swift, introduced: 4.0)
func swiftFourPointOh() {} // expected-note {{was introduced in Swift 4.0}}

@available(swift, introduced: 4.0, message: "uses abc")
func swiftFourPointOhWithMessage() {} // expected-note {{was introduced in Swift 4.0}}

@available(swift 4)
class SwiftShortFour {} // expected-note {{was introduced in Swift 4}}

@available(swift, introduced: 4, message: "uses pqr")
class SwiftFourWithMessage {} // expected-note {{was introduced in Swift 4}}


swiftShortThree()
swiftShortThreePointOh()
swiftThreePointOh()
swiftThreePointOhOnly()
swiftDeprecatedThreePointOh() // expected-warning {{is deprecated}}
swiftObsoletedThreePointOh() // expected-error {{is unavailable}}
let a : SwiftThreeOnly


swiftShortFour() // expected-error {{is unavailable}}
swiftShortFourPointOh() // expected-error {{is unavailable}}
swiftFour() // expected-error {{is unavailable}}
swiftFourPointOh() // expected-error {{is unavailable}}
swiftFourPointOhWithMessage() // expected-error {{is unavailable: uses abc}}
let aa : SwiftShortFour // expected-error {{is unavailable}}
let bb : SwiftFourWithMessage // expected-error {{is unavailable: uses pqr}}
