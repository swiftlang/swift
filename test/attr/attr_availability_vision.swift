// RUN: %swift -typecheck -verify -parse-stdlib -target arm64-apple-xros2.0 %s

@available(visionOS, introduced: 1.0, deprecated: 1.5, obsoleted: 2.0,
           message: "you don't want to do that anyway")
public func doSomething() { }
// expected-note @-1{{'doSomething()' was obsoleted in visionOS 2.0}}

doSomething() // expected-error{{'doSomething()' is unavailable in visionOS: you don't want to do that anyway}}

// Preservation of major.minor.micro
@available(visionOS, introduced: 1.0, deprecated: 1.5, obsoleted: 1.5.3)
func doSomethingElse() { }
// expected-note @-1{{'doSomethingElse()' was obsoleted in visionOS 1.5.3}}

doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable in visionOS}}

// Preservation of minor-only version
@available(visionOS, introduced: 1.0, deprecated: 1.5, obsoleted: 2)
func doSomethingReallyOld() { }
// expected-note @-1{{'doSomethingReallyOld()' was obsoleted in visionOS 2}}

doSomethingReallyOld() // expected-error{{'doSomethingReallyOld()' is unavailable in visionOS}}

// Test deprecations in 2.0 and later

@available(visionOS, introduced: 1.1, deprecated: 2.0,
           message: "Use another function")
func deprecatedFunctionWithMessage() { }

deprecatedFunctionWithMessage() // expected-warning{{'deprecatedFunctionWithMessage()' was deprecated in visionOS 2.0: Use another function}}


@available(visionOS, introduced: 1.0, deprecated: 2.0)
func deprecatedFunctionWithoutMessage() { }

deprecatedFunctionWithoutMessage() // expected-warning{{'deprecatedFunctionWithoutMessage()' was deprecated in visionOS 2.0}}

@available(visionOS, introduced: 1.0, deprecated: 2.0,
           message: "Use BetterClass instead")
class DeprecatedClass { }

func functionWithDeprecatedParameter(p: DeprecatedClass) { } // expected-warning{{'DeprecatedClass' was deprecated in visionOS 2.0: Use BetterClass instead}}

@available(visionOS, introduced: 2.0, deprecated: 4.0,
           message: "Use BetterClass instead")
class DeprecatedClassIn3_0 { }

// Elements deprecated later than the minimum deployment target (which is 2.0, in this case) should not generate warnings
func functionWithDeprecatedLaterParameter(p: DeprecatedClassIn3_0) { }

// Treat visionOS as an alias for iOS in availability queries

@available(visionOS, introduced: 2.2)
func functionIntroducedOnvisionOS2_2() { }

if #available(iOS 17.3, *) {
  functionIntroducedOnvisionOS2_2() // expected-error{{'functionIntroducedOnvisionOS2_2()' is only available in visionOS 2.2 or newer}}
  // expected-note @-1{{add 'if #available' version check}}
}

if #available(visionOS 1.5, *) {
  functionIntroducedOnvisionOS2_2() // expected-error{{'functionIntroducedOnvisionOS2_2()' is only available in visionOS 2.2 or newer}}
  // expected-note @-1{{add 'if #available' version check}}    
}

if #available(visionOS 2.2, *) {
  functionIntroducedOnvisionOS2_2()
}

@available(visionOS 1.50.4, *)
public func foo() { }
