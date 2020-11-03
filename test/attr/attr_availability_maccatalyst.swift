// RUN: %swift -typecheck -verify -parse-stdlib -target x86_64-apple-ios51.0-macabi %s

// REQUIRES: OS=maccatalyst

@available(macCatalyst, introduced: 1.0, deprecated: 2.0, obsoleted: 9.0,
           message: "you don't want to do that anyway")
func obsoletedOnMacCatalyst() { }
// expected-note @-1{{'obsoletedOnMacCatalyst()' was obsoleted in Mac Catalyst 9.0}}

obsoletedOnMacCatalyst() // expected-error{{'obsoletedOnMacCatalyst()' is unavailable in Mac Catalyst: you don't want to do that anyway}}

@available(iOS, introduced: 1.0, deprecated: 2.0, obsoleted: 9.0,
           message: "you don't want to do that anyway")
func obsoletedOnIOS() { }
// expected-note @-1{{'obsoletedOnIOS()' was obsoleted in iOS 9.0}}

obsoletedOnIOS() // expected-error{{'obsoletedOnIOS()' is unavailable in iOS: you don't want to do that anyway}}


@available(iOS, introduced: 1.0)
@available(macCatalyst, introduced: 1.0, obsoleted: 12.0)
func obsoletedOnMacCatalystButNotIOS() { }
// expected-note @-1{{'obsoletedOnMacCatalystButNotIOS()' was obsoleted in Mac Catalyst 12.0}}

obsoletedOnMacCatalystButNotIOS() // expected-error {{'obsoletedOnMacCatalystButNotIOS()' is unavailable}}


@available(iOS, introduced: 12.0, obsoleted: 12.0)
@available(macCatalyst, introduced: 12.0)
func obsoletedOnIOSButNotMacCatalyst() { }
obsoletedOnIOSButNotMacCatalyst() // no-error



@available(iOS, introduced: 1.0)
@available(macCatalyst, introduced: 1.0, deprecated: 12.0)
func deprecatedOnMacCatalystButNotIOS() { }

deprecatedOnMacCatalystButNotIOS() // expected-warning {{deprecatedOnMacCatalystButNotIOS()' was deprecated in Mac Catalyst 12.0}}

@available(iOS, introduced: 12.0, deprecated: 13.0)
@available(macCatalyst, introduced: 12.0)
func deprecatedOnIOSButNotMacCatalyst() { }
deprecatedOnIOSButNotMacCatalyst() // no-warning


@available(iOS 55.0, macCatalyst 56.0, *)
func introducedLaterOnMacCatalyst() {
}

@available(iOS 57.0, macCatalyst 56.0, *)
func introducedLaterOnIOS() {
}

// expected-note@+1 *{{add @available attribute to enclosing global function}}
func testPoundAvailable() {

  if #available(macCatalyst 55.0, *) {
    introducedLaterOnMacCatalyst() // expected-error {{'introducedLaterOnMacCatalyst()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    introducedLaterOnIOS() // expected-error {{'introducedLaterOnIOS()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }

  // macCatalyst should win over iOS when present

  if #available(iOS 56.0, macCatalyst 55.0, *) {
    introducedLaterOnMacCatalyst() // expected-error {{'introducedLaterOnMacCatalyst()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    introducedLaterOnIOS() // expected-error {{'introducedLaterOnIOS()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }

  if #available(iOS 55.0, macCatalyst 56.0, *) {
    introducedLaterOnMacCatalyst() // no-warning
    introducedLaterOnIOS() // no-error
  }

  if #available(iOS 57.0, macCatalyst 56.0, *) {
    introducedLaterOnMacCatalyst() // no-warning
    introducedLaterOnIOS() // no-error
  }

  // iOS availability should be inherited when macCatalyst is not present

  if #available(iOS 55.0, *) {
    introducedLaterOnMacCatalyst() // expected-error {{'introducedLaterOnMacCatalyst()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
    introducedLaterOnIOS() // expected-error {{'introducedLaterOnIOS()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }

  if #available(iOS 56.0, *) {
    introducedLaterOnMacCatalyst() // no-warning
    introducedLaterOnIOS() // no-error
  }

  // macOS availability doesn't count on macCatalyst for Swift.
  if #available(macOS 9999.0, *) {
    introducedLaterOnMacCatalyst() // expected-error {{'introducedLaterOnMacCatalyst()' is only available in Mac Catalyst 56.0 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}
  }
}

@available(iOS 55.0, *)
func testUnnecessaryPoundAvailable() { // expected-note*{{enclosing scope here}}

  // Even though we're compiling for macCatalyst, the #available is expressed in terms of
  // 'iOS', so we should use that to report to the user in the diagnostic.
  if #available(iOS 54.0, *) {
    // expected-warning@-1 {{unnecessary check for 'iOS'; enclosing scope ensures guard will always be true}}
  }

  if #available(macCatalyst 54.0, *) {
    // expected-warning@-1 {{unnecessary check for 'macCatalyst'; enclosing scope ensures guard will always be true}}
  }

  if #available(macCatalyst 54.0, iOS 53.0, *) {
    // expected-warning@-1 {{unnecessary check for 'macCatalyst'; enclosing scope ensures guard will always be true}}
  }

  if #available(iOS 53.0, macCatalyst 54.0, *) {
    // expected-warning@-1 {{unnecessary check for 'macCatalyst'; enclosing scope ensures guard will always be true}}
  }
}

// Test that we don't accidentally try to validate @available(iOS, ...) attrs
// on accessors against the property's @available(macCatalyst, ...) attr.
// (rdar://problem/50067784)
class X {
  @available(iOS 3.2, macCatalyst 13, *)
  var x: X {
    get { return self }
    set {  }
  }
}

protocol P: Builtin.AnyObject {
  var x: X { get set }
}

extension X: P {}

// Test platform inheritance for iOS unavailability.
// rdar://68597591

@available(iOS, unavailable)
public struct UnavailableOniOS { } // expected-note 2 {{'UnavailableOniOS' has been explicitly marked unavailable here}}

@available(iOS, unavailable)
func unavailableOniOS(_ p: UnavailableOniOS) { } // ok

func functionUsingAnUnavailableType(_ p: UnavailableOniOS) { } // expected-error {{'UnavailableOniOS' is unavailable in iOS}}

public extension UnavailableOniOS { } // expected-error {{'UnavailableOniOS' is unavailable in iOS}}

@available(iOS, unavailable)
public extension UnavailableOniOS { // ok
  func someMethod(_ p: UnavailableOniOS) { }
}

@available(iOS, unavailable)
@available(macCatalyst, introduced: 13.0)
public struct AvailableOnMacCatalyst { }

public extension AvailableOnMacCatalyst { } // ok
