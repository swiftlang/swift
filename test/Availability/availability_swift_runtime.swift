// RUN: %target-typecheck-verify-swift -parse-as-library -enable-experimental-feature StandaloneSwiftAvailability -min-swift-runtime-version 5.5

// REQUIRES: swift_feature_StandaloneSwiftAvailability

@available(Swift 5.0, *)
func availableInSwift5_0Runtime() { }

@available(Swift 5.5, *)
func availableInSwift5_5Runtime() { }

@available(Swift 6.0, *)
func availableInSwift6_0Runtime() { }

func alwaysAvailable() {
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}{{1-1=@available(Swift 6.0)\n}}

  availableInSwift5_0Runtime()
  availableInSwift5_5Runtime()
  availableInSwift6_0Runtime() // expected-error {{'availableInSwift6_0Runtime()' is only available in Swift 6.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-31=if #available(Swift 6.0) {\n      availableInSwift6_0Runtime()\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  if #available(Swift 6.0, *) {
    availableInSwift6_0Runtime()
  }
}

@available(Swift 6.0, *)
func availableSwift6() {
  availableInSwift6_0Runtime()
}

@available(Swift, introduced: 5.0, obsoleted: 5.1)
func obsoletedBeforeSwiftRuntime() {}
// expected-note@-1 3{{'obsoletedBeforeSwiftRuntime()' was obsoleted in Swift 5.1}}

func reachableUseStillDiagnosed() {
  obsoletedBeforeSwiftRuntime()
  // expected-error@-1 {{'obsoletedBeforeSwiftRuntime()' is unavailable in Swift}}
}

func useInUnreachableUnavailableBranch() {
  if #unavailable(Swift 5.1) {
    obsoletedBeforeSwiftRuntime()
    // expected-error@-1 {{'obsoletedBeforeSwiftRuntime()' is unavailable in Swift}}
    // FIXME: This error should be ignored https://github.com/swiftlang/swift/pull/88765
  }
}

func useInUnreachableAvailableElseBranch() {
  if #available(Swift 5.1, *) {
  } else {
    obsoletedBeforeSwiftRuntime()
    // expected-error@-1 {{'obsoletedBeforeSwiftRuntime()' is unavailable in Swift}}
    // FIXME: This error should be ignored https://github.com/swiftlang/swift/pull/88765
  }
}
