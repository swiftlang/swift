// RUN: %empty-directory(%t)

// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-bare-slash-regex -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
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

_ = [(Int) -> async throws Int]()
// expected-error@-1{{'async throws' must precede '->'}}
// expected-note@-2{{move 'async throws' in front of '->'}}{{15-21=}} {{21-28=}} {{12-12=async }} {{12-12=throws }}

@freestanding // expected-error {{expected arguments for 'freestanding' attribute}}
func dummy() {}

@_silgen_name("whatever", extra)  // expected-error@:27 {{unexpected arguments in '_silgen_name' attribute}}
func _whatever()

struct S {
    subscript(x: Int) { _ = 1 } // expected-error@:23 {{expected '->' and return type in subscript}}
                                // expected-note@-1:23 {{insert '->' and return type}}
}

struct ExpansionRequirementTest<each T> {}
extension ExpansionRequirementTest where repeat each T == Int {} // expected-error {{same-element requirements are not yet supported}}
