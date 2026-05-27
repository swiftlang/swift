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
