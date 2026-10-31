// A version that is all zeros is not a version: warn (until Swift 6) and drop
// the specification, in both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: OS=macosx

func f() {
  if #available(macOS 0, *) {} // expected-warning {{expected version number; this is an error in the Swift 6 language mode}}
  if #available(macOS 0.0, *) {} // expected-warning {{expected version number; this is an error in the Swift 6 language mode}}
}
