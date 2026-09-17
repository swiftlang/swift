// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-macosx14.0 -parse-stdlib -D EXPECT_FALLBACK -D EXPECT_ANY_APPLE_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-macosx15.0 -parse-stdlib -D EXPECT_PRIMARY -D EXPECT_ANY_APPLE_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-ios17.0 -parse-stdlib -D EXPECT_FALLBACK -D EXPECT_ANY_APPLE_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-ios18.0 -parse-stdlib -D EXPECT_PRIMARY -D EXPECT_ANY_APPLE_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-ios17.0-macabi -parse-stdlib -D EXPECT_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-ios18.0-macabi -parse-stdlib -D EXPECT_PRIMARY
// RUN: %swift -typecheck %s -verify -verify-additional-prefix linux- -enable-experimental-feature DeploymentTargetCondition -target x86_64-unknown-linux-gnu -parse-stdlib -D EXPECT_PRIMARY
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target aarch64-unknown-linux-android28 -parse-stdlib -D EXPECT_FALLBACK
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target aarch64-unknown-linux-android29 -parse-stdlib -D EXPECT_PRIMARY
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target x86_64-pc-windows10.0.19041-msvc -parse-stdlib -D EXPECT_PRIMARY
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-macosx26.0 -parse-stdlib -D EXPECT_PRIMARY -D EXPECT_ANY_APPLE_PRIMARY
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-ios26.0 -parse-stdlib -D EXPECT_PRIMARY -D EXPECT_ANY_APPLE_PRIMARY
// RUN: %swift -typecheck %s -verify -enable-experimental-feature DeploymentTargetCondition -target arm64-apple-xros2.0 -parse-stdlib -D EXPECT_PRIMARY -D EXPECT_ANY_APPLE_FALLBACK

#if !hasFeature(DeploymentTargetCondition)
  #error("expected DeploymentTargetCondition to be enabled")
#endif

#if deploymentTargetAtLeast(macOS 15, iOS 18, Android 29, Windows 10, *)
  #if EXPECT_FALLBACK
    #error("expected the fallback implementation")
  #endif
#else
  #if EXPECT_PRIMARY
    #error("expected the primary implementation")
  #endif
#endif

#if deploymentTargetAtLeast(anyAppleOS 26, *)
  #if EXPECT_ANY_APPLE_FALLBACK
    #error("expected the anyAppleOS fallback implementation")
  #endif
#else
  #if EXPECT_ANY_APPLE_PRIMARY
    #error("expected the anyAppleOS primary implementation")
  #endif
#endif

// A platform-specific requirement takes precedence over anyAppleOS.
#if os(macOS)
  #if deploymentTargetAtLeast(anyAppleOS 26, macOS 15, *)
    #if EXPECT_FALLBACK
      #error("expected the macOS-specific requirement")
    #endif
  #else
    #if EXPECT_PRIMARY
      #error("expected the macOS-specific requirement")
    #endif
  #endif
#endif

// A target-environment requirement takes precedence over its operating system.
#if targetEnvironment(macCatalyst)
  #if deploymentTargetAtLeast(iOS 99, macCatalyst 18, *)
    #if EXPECT_FALLBACK
      #error("expected the macCatalyst-specific requirement")
    #endif
  #else
    #if EXPECT_PRIMARY
      #error("expected the macCatalyst-specific requirement")
    #endif
  #endif
#endif

// A requirement applies only to the platform it names. Availability checking
// can apply an iOS requirement to visionOS by remapping the version through the
// SDK, but that information is not available while '#if' is evaluated, so an
// iOS requirement does not constrain a visionOS target; '*' applies instead.
#if os(visionOS)
  #if !deploymentTargetAtLeast(iOS 99, *)
    #error("an iOS requirement must not constrain a visionOS target")
  #endif

  // A requirement naming visionOS is compared against the deployment target.
  #if deploymentTargetAtLeast(visionOS 99, *)
    #error("expected the visionOS requirement to be compared")
  #endif
#endif

#if deploymentTargetAtLeast(Linux 6, *)
// expected-linux-error@-1 {{deployment target version is unavailable for platform 'Linux'}}
#endif

#if deploymentTargetAtLeast(macOS 15)
// expected-error@-1 {{'deploymentTargetAtLeast' must handle potential future platforms with '*'}}
#endif

#if deploymentTargetAtLeast(*, macOS 15)
// expected-error@-1 {{'*' must be the last argument to 'deploymentTargetAtLeast'}}
#endif

#if deploymentTargetAtLeast(macOS 15, macOS 16, *)
// expected-error@-1 {{deployment target for platform 'macOS' was already specified}}
#endif

#if deploymentTargetAtLeast(macOS 15, OSX 16, *)
// expected-error@-1 {{deployment target for platform 'OSX' was already specified}}
#endif

#if deploymentTargetAtLeast(15, *)
// expected-error@-1 {{'deploymentTargetAtLeast' expects platform-version pairs followed by '*'}}
#endif

#if deploymentTargetAtLeast(macOZ 15, *)
// expected-warning@-1 {{unknown deployment target platform 'macOZ'}}
#endif

#if deploymentTargetAtLeast(anyAppleOS 25, *)
// expected-warning@-1 {{'25' is not a valid version number for any Apple OS}}
#endif
