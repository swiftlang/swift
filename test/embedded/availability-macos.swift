// Checks that in Embedded Swift, we allow using stdlib types even when they
// have declared availability to be higher than our macOS deployment target.

// RUN: %target-typecheck-verify-swift -target arm64-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature Span
// RUN: %target-typecheck-verify-swift -target arm64-apple-macos15 -enable-experimental-feature Embedded -enable-experimental-feature Span

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Span
// REQUIRES: OS=macosx

func f(_: Span<Int>) { } // Span is @available(SwiftStdlib 6.1, *)
func g(_: Int128) { } // Int128 is @available(SwiftStdlib 6.0, *)
