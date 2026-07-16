// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}frt-constructors.h

import FRTConstructors

@available(SwiftStdlib 5.8, *)
func ownershipAnnotations() {
  let _ = FRTImplicitDefaultCtor1() // expected-warning {{cannot infer ownership of foreign reference value}}
  let _ = FRTImplicitDefaultCtor0() // expected-warning {{cannot infer ownership of foreign reference value}}

  let _ = FRTExplicitDefaultCtorNoAnnotation() // expected-warning {{cannot infer ownership of foreign reference value}}
  let _ = FRTExplicitDefaultCtor1()
  let _ = FRTExplicitDefaultCtor0()

  let _ = FRTUserDefaultCtorNoAnnotation() // expected-warning {{cannot infer ownership of foreign reference value}}
  let _ = FRTUserDefaultCtor1()
  let _ = FRTUserDefaultCtor0()

  let _ = FRTMixedConventionCtors()
  let _ = FRTMixedConventionCtors(0)
  let _ = FRTMixedConventionCtors(0, 0) // expected-warning {{cannot infer ownership of foreign reference value}}

  let _ = FRTMixedConventionCtorsUnretainedByDefault()
  let _ = FRTMixedConventionCtorsUnretainedByDefault(0)
  let _ = FRTMixedConventionCtorsUnretainedByDefault(0, 0)
}

@available(SwiftStdlib 5.8, *)
func ctorWithAvailabilityAttr() {
  // A constructor marked `availability(swift, unavailable)` makes the synthesized
  // initializer unavailable in Swift.
  let _ = FRTUnavailableCtor() // expected-error {{'init()' is unavailable in Swift: cannot use this constructor}}

  // Only the unavailable overload is rejected; the available one still works.
  let _ = FRTMixedAvailabilityCtors()     // OK
  let _ = FRTMixedAvailabilityCtors(1)    // expected-error {{'init(_:)' is unavailable in Swift: cannot construct from an int}}
  let _ = FRTMixedAvailabilityCtors(1, 2) // expected-error {{'init(_:_:)' is unavailable: cannot construct from two ints}}
  let _ = FRTMixedAvailabilityCtors(1, 2, 3) // expected-warning {{'init(_:_:_:)' is deprecated: don't construct from three ints}}
}
