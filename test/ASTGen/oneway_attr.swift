// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_SendCalls

// rdar://116686158
// UNSUPPORTED: asan

// The '@oneway' and '@oneway(never)' declaration attributes are ordinary
// attributes, which ASTGen handles like the C++ parser

@MainActor @oneway func ping() {}
@MainActor @oneway func pingAsync() async {}

actor Worker {
  @oneway func tick() {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway nonisolated func log(_ line: String) {}
  @oneway(never) func drain() {}
}
