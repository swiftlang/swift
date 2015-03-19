// RUN: %swift -parse -verify -parse-stdlib -target i386-apple-tvos9.0 %s

@availability(tvOS, introduced=1.0, deprecated=2.0, obsoleted=9.0,
              message="you don't want to do that anyway")
func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in tvOS version 9.0}}

doSomething() // expected-error{{'doSomething()' is unavailable: you don't want to do that anyway}}

// Preservation of major.minor.micro
@availability(tvOS, introduced=1.0, deprecated=2.0, obsoleted=8.0)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in tvOS version 8.0}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable}}

// Preservation of minor-only version
@availability(tvOS, introduced=1.0, deprecated=1.5, obsoleted=9)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in tvOS version 9}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable}}

// Test deprecations in 9.0 and later

@availability(tvOS, introduced=1.1, deprecated=9.0,
              message="Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in tvOS version 9.0: Use another function}}


@availability(tvOS, introduced=1.0, deprecated=9.0)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in tvOS version 9.0}}

@availability(tvOS, introduced=1.0, deprecated=9.0,
              message="Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in tvOS version 9.0: Use BetterClass instead}}

@availability(tvOS, introduced=7.0, deprecated=10.0,
              message="Use BetterClass instead")
class DeprecatedClassIn8_0 { }

// Elements deprecated later than the minimum deployment target (which is 9.0, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn8_0) { }
