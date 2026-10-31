// Platform versions in '@backDeployed' and '@_originallyDefinedIn', checked
// against both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library
// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: OS=macosx

@backDeployed(before: macOS 0) // expected-warning {{expected version number in '@backDeployed' attribute; this is an error in the Swift 6 language mode}}
public func zero() {}

@backDeployed(before: macOS) // expected-error {{expected version number in '@backDeployed' attribute}}
public func missing() {}

@backDeployed(before: macOS 12.0.1) // expected-warning {{'@backDeployed' only uses major and minor version number}}
public func patch() {}

@backDeployed(before: macOS 12.0, iOS 15.0)
public func twoPlatforms() {}
