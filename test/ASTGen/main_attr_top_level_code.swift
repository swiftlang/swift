// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-ast -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-ast \
// RUN:    | %sanitize-address > %t/cpp-parser.ast
// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-swift-frontend-dump-ast -DHIDE_MAIN_TYPE -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.not-main.ast
// RUN: %target-swift-frontend-dump-ast -DHIDE_MAIN_TYPE \
// RUN:    | %sanitize-address > %t/cpp-parser.not-main.ast
// RUN: %diff -u %t/astgen.not-main.ast %t/cpp-parser.not-main.ast

// RUN: %target-swift-frontend-dump-ast -parse-as-library -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.library.ast
// RUN: %target-swift-frontend-dump-ast -parse-as-library \
// RUN:    | %sanitize-address > %t/cpp-parser.library.ast
// RUN: %diff -u %t/astgen.library.ast %t/cpp-parser.library.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -DHIDE_MAIN_TYPE -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -DHIDE_MAIN_TYPE

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// ASTGen and the C++ parser handle @main in top level code differently, ensure they produce equivalent ASTs, especially
// when the C++ parser needs to fix up declarations of globals after encountering @main.

func asyncFunc() async -> Int { 0 }
func takesClosure(_ f: () -> Int) -> Int { f() }
let ifExpr = if Bool.random() { takesClosure { 1 } } else { 2 }
let nested = takesClosure { takesClosure { 3 } }
let first = takesClosure { 1 }, second = takesClosure { 2 }
#if HIDE_MAIN_TYPE
let asyncValue = await asyncFunc()
#endif

#if !HIDE_MAIN_TYPE
@main
struct App {
  static func main() {
    print(ifExpr, nested, first, second)
  }
}
#endif
