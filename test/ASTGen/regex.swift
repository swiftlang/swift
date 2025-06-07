// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse -enable-bare-slash-regex -disable-availability-checking -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse -enable-bare-slash-regex -disable-availability-checking \
// RUN:    | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// rdar://116686158
// UNSUPPORTED: asan

func testRegexLiteral() {
  _ = /abc/
  _ = #/abc/#
  _ = ##/abc/##

  func foo<T>(_ x: T...) {}
  foo(/abc/, #/abc/#, ##/abc/##)

  let _ = [/abc/, #/abc/#, ##/abc/##]

  _ = /\w+/.self
  _ = #/\w+/#.self
  _ = ##/\w+/##.self

  _ = /#\/\#\\/
  _ = #/#/\/\#\\/#
  _ = ##/#|\|\#\\/##
}


