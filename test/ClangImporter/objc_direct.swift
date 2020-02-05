// RUN: %target-swift-frontend-verify -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/objc_direct.h -verify-ignore-unknown

// REQUIRES: objc_interop

func callThingsOnBar(_ x: Bar) {
  let _ = x.directProperty  // expected-error {{getter for 'directProperty' is unavailable in Swift}}
  let _ = x.directProperty2 // expected-error {{getter for 'directProperty2' is unavailable in Swift}}

  x.directMethod() // expected-error {{'directMethod()' is unavailable in Swift}}
  x.directMethod2() // expected-error {{'directMethod2()' is unavailable in Swift}}

  Bar.directClassMethod() // expected-error {{'directClassMethod()' is unavailable in Swift}}
  Bar.directClassMethod2() // expected-error {{'directClassMethod2()' is unavailable in Swift}}
}
