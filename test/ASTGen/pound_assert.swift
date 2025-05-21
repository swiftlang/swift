// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature StaticAssert \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature StaticAssert \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-swift-emit-sil -verify %s -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature StaticAssert

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_StaticAssert

func testPoundAssert() {
#assert(1 == 1.0, "1 must be 1.0") // OK
#assert(1 == 1.5, "1 is 1.5 ??") // expected-error {{1 is 1.5 ??}}
}
