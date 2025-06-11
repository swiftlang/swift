// RUN: %empty-directory(%t)

// RUN: %target-typecheck-verify-swift -disable-availability-checking \
// RUN:   -enable-bare-slash-regex \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_DefaultIsolationPerFile

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


#warning("this is a warning") // expected-warning {{this is a warning}}

func testDiagnosticInFunc() {
  #error("this is an error") // expected-error {{this is an error}}
}

class TestDiagnosticInNominalTy {
  #error("this is an error member") // expected-error {{this is an error member}}
}

#if FLAG_NOT_ENABLED
  #error("error in inactive") // no diagnostis
#endif

func misisngExprTest() {
  func fn(x: Int, y: Int) {}
  fn(x: 1, y:) // expected-error {{expected value in function call}}
               // expected-note@-1 {{insert value}} {{14-14= <#expression#>}}
}

func misisngTypeTest() {
  func fn() -> {} // expected-error {{expected return type in function signature}}
                  // expected-note@-1 {{insert return type}} {{16-16=<#type#> }}
}
func misisngPatternTest(arr: [Int]) {
  for {} // expected-error {{expected pattern, 'in', and expression in 'for' statement}}
         // expected-note@-1 {{insert pattern, 'in', and expression}} {{7-7=<#pattern#> }} {{7-7=in }} {{7-7=<#expression#> }}
}

using @MainActor // expected-note {{default isolation was previously declared here}}
using nonisolated // expected-error {{invalid redeclaration of file-level default actor isolation}}

using @Test
// expected-error@-1 {{default isolation can only be set to '@MainActor' or 'nonisolated'}}

using test
// expected-error@-1 {{default isolation can only be set to '@MainActor' or 'nonisolated'}}