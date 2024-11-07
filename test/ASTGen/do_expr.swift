// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-feature DoExpressions -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-feature DoExpressions > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

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
