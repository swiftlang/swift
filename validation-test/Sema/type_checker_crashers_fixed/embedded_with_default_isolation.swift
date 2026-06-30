// RUN: %target-typecheck-verify-swift -target arm64-apple-macos14 -enable-experimental-feature Embedded -default-isolation MainActor

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

var counter = 0

func tick() {
  counter += 1
}
