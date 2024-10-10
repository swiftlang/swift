// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -enable-bare-slash-regex -disable-availability-checking -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -enable-bare-slash-regex -disable-availability-checking > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_swift_parser

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
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


