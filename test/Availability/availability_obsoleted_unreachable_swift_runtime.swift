// RUN: %target-typecheck-verify-swift -parse-as-library -enable-experimental-feature StandaloneSwiftAvailability -min-swift-runtime-version 7.0
// REQUIRES: swift_feature_StandaloneSwiftAvailability

@available(Swift, introduced: 5.0, obsoleted: 6.0)
func obsoletedBeforeSwiftRuntime() {}
// expected-note@-1 {{'obsoletedBeforeSwiftRuntime()' was obsoleted in Swift 6.0}}

func reachableUseStillDiagnosed() {
  obsoletedBeforeSwiftRuntime()
  // expected-error@-1 {{'obsoletedBeforeSwiftRuntime()' is unavailable in Swift}}
}

func useInUnreachableUnavailableBranch() {
  if #unavailable(Swift 6.0) {
    obsoletedBeforeSwiftRuntime()
  }
}

func useInUnreachableAvailableElseBranch() {
  if #available(Swift 6.0, *) {
  } else {
    obsoletedBeforeSwiftRuntime()
  }
}
