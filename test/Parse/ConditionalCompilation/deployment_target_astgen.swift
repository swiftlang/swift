// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -enable-experimental-feature ParserASTGen -target arm64-apple-xros1.0 -sdk %S/../../attr/Inputs/XROS1.1.sdk -parse-stdlib -D EXPECT_BELOW
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -enable-experimental-feature ParserASTGen -target arm64-apple-xros1.1 -sdk %S/../../attr/Inputs/XROS1.1.sdk -parse-stdlib -D EXPECT_AT_LEAST
// REQUIRES: swift_feature_ParserASTGen

#if !deploymentTargetAtLeast(iOS 17.4, *)
  #error("expected the wildcard for an iOS-only requirement")
#endif

#if deploymentTargetAtLeast(visionOS 1.1, *)
  #if EXPECT_BELOW
    #error("expected the visionOS requirement to fail")
  #endif
#else
  #if EXPECT_AT_LEAST
    #error("expected the visionOS requirement to pass")
  #endif
#endif

#if deploymentTargetAtLeast(xrOS 1.1.0.0.0.0, *)
  #if EXPECT_BELOW
    #error("expected the xrOS requirement to fail")
  #endif
#else
  #if EXPECT_AT_LEAST
    #error("expected the xrOS requirement to pass")
  #endif
#endif
