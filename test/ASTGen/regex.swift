// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse \
// RUN:    -target %target-swift-5.7-abi-triple \
// RUN:    -enable-bare-slash-regex -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse \
// RUN:    -target %target-swift-5.7-abi-triple \
// RUN:    -enable-bare-slash-regex \
// RUN:    | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift \
// RUN:    -target %target-swift-5.7-abi-triple \
// RUN:    -enable-experimental-feature ParserASTGen -enable-bare-slash-regex

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


