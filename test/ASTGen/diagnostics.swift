// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend %s -dump-parse -target %target-swift-5.1-abi-triple -enable-bare-slash-regex -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: not %target-swift-frontend %s -dump-parse -target %target-swift-5.1-abi-triple -enable-bare-slash-regex > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-bare-slash-regex -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

func testRegexLiteral() {
  _ = (#/[*/#, #/+]/#, #/.]/#)
  // expected-error@-1:18 {{cannot parse regular expression: quantifier '+' must appear after expression}}
  // expected-error@-2:12 {{cannot parse regular expression: expected ']'}}
}

func testEditorPlaceholder() -> Int {
  func foo(_ x: String) {}
  foo(<#T##x: String##String#>) // expected-error {{editor placeholder in source file}})
  return <#T##Int#> // expected-error {{editor placeholder in source file}}
}
