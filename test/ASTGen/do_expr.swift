// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-feature DoExpressions -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-feature DoExpressions \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature DoExpressions -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_DoExpressions
// REQUIRES: swift_feature_ParserASTGen
// rdar://116686158
// UNSUPPORTED: asan

func fn() async throws -> Int { 6 }
struct Err: Error {}

func testDoExpr() async throws {
  let _: Int = do { 5 }
  let _: Int = do { try await fn() } catch { throw Err() }
}
