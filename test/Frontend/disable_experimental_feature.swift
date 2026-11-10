// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ParserASTGen -disable-experimental-feature ParserASTGen -verify
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ParserASTGen 2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: asserts

// `-disable-experimental-feature` turns a feature off, including one that was
// enabled earlier on the command line (or by default in the build).

#if hasFeature(ParserASTGen)
#error("ParserASTGen is on")
// CHECK: ParserASTGen is on
#endif
