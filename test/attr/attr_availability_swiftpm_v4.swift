// RUN: %target-typecheck-verify-swift -package-description-version 4.0

@available(_PackageDescription 3)
func shortThree() {}

@available(_PackageDescription, introduced: 3.0)
func threePointOh() {}

@available(_PackageDescription, introduced: 3.0, obsoleted: 4.0)
func threePointOhOnly() {} // expected-note {{was obsoleted in PackageDescription 4.0}}

@available(_PackageDescription, deprecated: 3.0)
func deprecatedThreePointOh() {}

@available(_PackageDescription, obsoleted: 3.0)
func obsoletedThreePointOh() {} // expected-note {{was obsoleted in PackageDescription 3.0}}

@available(_PackageDescription, introduced: 3.0, obsoleted: 4.0)
class ThreePointOhOnly {} // expected-note {{was obsoleted in PackageDescription 4.0}}

@available(_PackageDescription, introduced: 3, obsoleted: 4, message: "use abc")
class ThreeOnlyWithMessage {} // expected-note {{was obsoleted in PackageDescription 4}}


@available(_PackageDescription 4)
func shortFour() {}

@available(_PackageDescription 4.0)
func shortFourPointOh() {}

@available(_PackageDescription, introduced: 4)
func four() {}

@available(_PackageDescription, introduced: 4.0)
func fourPointOh() {}

@available(_PackageDescription 4)
class ShortFour {}

shortThree()
threePointOh()
threePointOhOnly() // expected-error {{is unavailable}}
deprecatedThreePointOh() // expected-warning {{is deprecated}}
obsoletedThreePointOh() // expected-error {{is unavailable}}
let a : ThreePointOhOnly // expected-error {{is unavailable}}
let b : ThreeOnlyWithMessage // expected-error {{is unavailable: use abc}}


shortFour()
shortFourPointOh()
four()
fourPointOh()
let aa : ShortFour

@available(_PackageDescription, introduced: 4.0)
@available(*, deprecated, message: "test deprecated")
func unconditionallyDeprecated() {}

unconditionallyDeprecated() // expected-warning {{test deprecated}}

@available(_PackageDescription 4.0, iOS 2.0, *) // expected-error {{'_PackageDescription' version-availability must be specified alone}}
func shouldBeAlone() {} 

@available(_PackageDescription 4.0, swift 2.0, *) // expected-error {{'_PackageDescription' version-availability must be specified alone}} // expected-error {{'swift' version-availability must be specified alone}}
func shouldBeAlone2() {} 
