// RUN: %swift -typecheck -primary-file %s %S/Inputs/availability_multi_other.swift -verify
// REQUIRES: OS=macosx

func callToEnsureNotInScriptMode() { }
// Add an expected error to express expectation that we're not in script mode
callToEnsureNotInScriptMode() // expected-error {{expressions are not allowed at the top level}}

@available(OSX, introduced: 10.9)
var globalAvailableOn10_9: Int = 9

@available(OSX, introduced: 10.51)
var globalAvailableOn10_51: Int = 10

@available(OSX, introduced: 10.52)
var globalAvailableOn10_52: Int = 11

// Top level should reflect the minimum deployment target.
let ignored1: Int = globalAvailableOn10_9

let ignored2: Int = globalAvailableOn10_51 // expected-error {{'globalAvailableOn10_51' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing let}}

let ignored3: Int = globalAvailableOn10_52 // expected-error {{'globalAvailableOn10_52' is only available on OS X 10.52 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing let}}

@available(OSX, introduced: 10.51)
func useFromOtherOn10_51() {
  // This will trigger validation of OtherIntroduced10_51 in
  // in availability_multi_other.swift
  let o10_51 = OtherIntroduced10_51()
  o10_51.extensionMethodOnOtherIntroduced10_51()

  let o10_9 = OtherIntroduced10_9()
  o10_9.extensionMethodOnOtherIntroduced10_9AvailableOn10_51(o10_51)
  _ = o10_51.returns10_52Introduced10_52() // expected-error {{'returns10_52Introduced10_52()' is only available on OS X 10.52 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  _ = OtherIntroduced10_52()
      // expected-error@-1 {{'OtherIntroduced10_52' is only available on OS X 10.52 or newer}}
      // expected-note@-2 {{add 'if #available' version check}}

  o10_51.extensionMethodOnOtherIntroduced10_51AvailableOn10_52() // expected-error {{'extensionMethodOnOtherIntroduced10_51AvailableOn10_52()' is only available on OS X 10.52 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  _ = OtherIntroduced10_51.NestedIntroduced10_52()
      // expected-error@-1 {{'NestedIntroduced10_52' is only available on OS X 10.52 or newer}}
      // expected-note@-2 {{add 'if #available' version check}}
}

@available(OSX, introduced: 10.52)
func useFromOtherOn10_52() {
  _ = OtherIntroduced10_52()

  let n10_52 = OtherIntroduced10_51.NestedIntroduced10_52()
  _ = n10_52.returns10_52()
  _ = n10_52.returns10_53() // expected-error {{'returns10_53()' is only available on OS X 10.53 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  // This will trigger validation of the global in availability_in_multi_other.swift
  _ = globalFromOtherOn10_52
}
