// Checks shared by both parsers: 'try' must precede 'await'.

// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library
// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func tryAwait() async throws {
  _ = await try tryAwait() // expected-warning {{'try' must precede 'await'}}
}
