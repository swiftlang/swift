// RUN: %swift -typecheck %s -verify -target arm64-apple-macosx15.0 -parse-stdlib

#if hasFeature(DeploymentTargetCondition)
  #if deploymentTargetAtLeast(macOS 15, *)
    #error("disabled condition should not be evaluated")
  #endif
#endif

#if deploymentTargetAtLeast(macOS 15, *)
// expected-error@-1 {{'deploymentTargetAtLeast' is an experimental feature that is currently disabled}}
#endif
