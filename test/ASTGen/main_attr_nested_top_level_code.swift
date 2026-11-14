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

// The '@main' type may be nested inside another declaration, or even inside a
// function body. ASTGen searches the syntax tree for it while the C++ parser
// inspects the AST it has built so far, so make sure the two agree about
// whether top-level code is suppressed.

func asyncFunc() async -> Int { 0 }
func takesClosure(_ f: () -> Int) -> Int { f() }
let ifExpr = if Bool.random() { takesClosure { 1 } } else { 2 }
let nested = takesClosure { takesClosure { 3 } }
#if HIDE_MAIN_TYPE
let asyncValue = await asyncFunc()
#endif

enum Outer {
  enum Middle {
#if !HIDE_MAIN_TYPE
    @main
#endif
    struct App {
      static func main() {
        print(ifExpr, nested)
        nestsAnEntryPoint()
      }
    }
  }
}

func nestsAnEntryPoint() {
#if NEVER
  @main
  struct Unused {
    static func main() {}
  }
#endif
}
