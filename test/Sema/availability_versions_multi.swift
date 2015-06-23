// RUN: %swift -target x86_64-apple-macosx10.9 -parse -primary-file %s %S/Inputs/availability_multi_other.swift -verify
// REQUIRES: OS=macosx
// This test requires a minimum deployment target of exactly OS X 10.9 to properly
// check availability_multi_other.swift

func callToEnsureNotInScriptMode() { }
// Add an expected error to express expectation that we're not in script mode
callToEnsureNotInScriptMode() // expected-error {{expressions are not allowed at the top level}}

@available(OSX, introduced=10.9)
var globalAvailableOn10_9: Int = 9

@available(OSX, introduced=10.10)
var globalAvailableOn10_10: Int = 10

@available(OSX, introduced=10.11)
var globalAvailableOn10_11: Int = 11

// Top level should reflect the minimum deployment target.
let ignored1: Int = globalAvailableOn10_9

let ignored2: Int = globalAvailableOn10_10 // expected-error {{'globalAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing let}}

let ignored3: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X 10.11 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing let}}

@available(OSX, introduced=10.10)
func useFromOtherOn10_10() {
  // This will trigger validation of OtherIntroduced10_10 in
  // in availability_multi_other.swift
  let o10_10 = OtherIntroduced10_10()
  o10_10.extensionMethodOnOtherIntroduced10_10()

  let o10_9 = OtherIntroduced10_9()
  o10_9.extensionMethodOnOtherIntroduced10_9AvailableOn10_10(o10_10)
  o10_10.returns10_11Introduced10_11() // expected-error {{'returns10_11Introduced10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}

  _ = OtherIntroduced10_11() // expected-error {{'OtherIntroduced10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}

  o10_10.extensionMethodOnOtherIntroduced10_10AvailableOn10_11() // expected-error {{'extensionMethodOnOtherIntroduced10_10AvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}

  _ = OtherIntroduced10_10.NestedIntroduced10_11() // expected-error {{'NestedIntroduced10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}
}

@available(OSX, introduced=10.11)
func useFromOtherOn10_11() {
  _ = OtherIntroduced10_11()

  let n10_11 = OtherIntroduced10_10.NestedIntroduced10_11()
  n10_11.returns10_11()
  n10_11.returns10_12() // expected-error {{'returns10_12()' is only available on OS X 10.12 or newer}}
      // expected-note@-1 {{guard with version check}}

  // This will trigger validation of the global in availability_in_multi_other.swift
  _ = globalFromOtherOn10_11
}
