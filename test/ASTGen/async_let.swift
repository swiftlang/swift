// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// `async let _ = ...` has no VarDecl, so the wildcard pattern records it.
func discarded() async {
  @Sendable func someTask() async {}
  async let _ = someTask()
  async let _: Void = someTask()
  async let named = someTask()
  await named
}
