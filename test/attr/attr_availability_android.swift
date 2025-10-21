// RUN: %swift -typecheck -verify -parse-stdlib -target aarch64-unknown-linux-android28 %s

@available(Android, introduced: 1.0, deprecated: 2.0, obsoleted: 28.0,
              message: "you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in Android 28.0}}

doSomething() // expected-error{{'doSomething()' is unavailable in Android: you don't want to do that anyway}}

// Preservation of major.minor.micro
@available(Android, introduced: 1.0, deprecated: 2.0, obsoleted: 27.0)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in Android 27.0}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable in Android}}

// Test deprecations in 28.0 and later

@available(Android, introduced: 1.1, deprecated: 28.0,
              message: "Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in Android 28.0: Use another function}}


@available(Android, introduced: 1.0, deprecated: 28.0)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in Android 28.0}}

@available(Android, introduced: 1.0, deprecated: 28.0,
              message: "Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in Android 28.0: Use BetterClass instead}}

@available(Android, introduced: 7.0, deprecated: 29,
              message: "Use BetterClass instead")
class DeprecatedClassIn29_0 { }

// Elements deprecated later than the minimum deployment target (which is 28.0, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn29_0) { }

@available(Android, introduced: 30)
func functionIntroducedOnAndroid30() { }

if #available(iOS 17.3, *) {
  functionIntroducedOnAndroid30() // expected-error{{'functionIntroducedOnAndroid30()' is only available in Android 30 or newer}}
  // expected-note @-1{{add 'if #available' version check}}{{3-34=if #available(Android 30, *) {\n      functionIntroducedOnAndroid30()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
}

if #available(Android 28, *) {
  functionIntroducedOnAndroid30() // expected-error{{'functionIntroducedOnAndroid30()' is only available in Android 30 or newer}}
  // expected-note @-1{{add 'if #available' version check}}{{3-34=if #available(Android 30, *) {\n      functionIntroducedOnAndroid30()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
}

if #available(Android 30, *) {
  functionIntroducedOnAndroid30()
}
