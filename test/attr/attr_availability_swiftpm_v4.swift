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

@available(_PackageDescription 99)
func ninetyNine() {} // expected-note {{'ninetyNine()' was introduced in PackageDescription 99}}

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
ninetyNine() // expected-error {{'ninetyNine()' is unavailable}}

@available(_PackageDescription, introduced: 4.0)
@available(*, deprecated, message: "test deprecated")
func unconditionallyDeprecated() {}

unconditionallyDeprecated() // expected-warning {{test deprecated}}

@available(_PackageDescription 4.0, iOS 2.0, *) // expected-error {{PackageDescription version availability must be specified alone}}
func shouldBeAlone() {}

@available(_PackageDescription 4.0, swift 2.0, *) // expected-error {{PackageDescription version availability must be specified alone}} // expected-error {{Swift version availability must be specified alone}}
func shouldBeAlone2() {}

@available(*, unavailable, renamed: "shortFour")
@available(_PackageDescription 3)
func unconditionallyRenamed() {} // expected-note {{'unconditionallyRenamed()' has been explicitly marked unavailable here}}

unconditionallyRenamed() // expected-error {{'unconditionallyRenamed()' has been renamed to 'shortFour'}}

@available(*, unavailable, renamed: "shortFour")
@available(_PackageDescription 5)
func unconditionallyRenamedAndIntroducedLater() {} // expected-note {{'unconditionallyRenamedAndIntroducedLater()' has been explicitly marked unavailable here}}

unconditionallyRenamedAndIntroducedLater() // expected-error {{'unconditionallyRenamedAndIntroducedLater()' has been renamed to 'shortFour'}}

func testQuery() {
  if #available(_PackageDescription 4.0) { // expected-error {{PackageDescription version checks not allowed in #available(...)}}
    // expected-error@-1 {{condition required for target platform}}
    shortFourPointOh()
  }
}
