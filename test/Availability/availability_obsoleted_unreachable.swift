// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx50 -disable-objc-attr-requires-foundation-module
// REQUIRES: OS=macosx

@available(macOS, introduced: 10, obsoleted: 14)
func obsoletedBeforeDeploymentTarget() {}
// expected-note@-1 {{'obsoletedBeforeDeploymentTarget()' was obsoleted in macOS 14}}

func reachableUseStillDiagnosed() {
  obsoletedBeforeDeploymentTarget()
  // expected-error@-1 {{'obsoletedBeforeDeploymentTarget()' is unavailable in macOS}}
}

func useInUnreachableUnavailableBranch() {
  if #unavailable(macOS 14) {
    obsoletedBeforeDeploymentTarget()
  }
}

func useInUnreachableAvailableElseBranch() {
  if #available(macOS 14, *) {
  } else {
    obsoletedBeforeDeploymentTarget()
  }
}
