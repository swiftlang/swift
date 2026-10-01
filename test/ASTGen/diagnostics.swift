// RUN: %empty-directory(%t)

// RUN: %target-typecheck-verify-swift \
// RUN:   -target %target-swift-5.9-abi-triple \
// RUN:   -enable-bare-slash-regex \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature DefaultIsolationPerFile \
// RUN:   -enable-experimental-feature ScopeRestrictions

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_DefaultIsolationPerFile
// REQUIRES: swift_feature_ScopeRestrictions

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

  // Make sure we don't try to parse this as an editor placeholder.
  _ = `<#foo#>` // expected-error {{cannot find '<#foo#>' in scope}}

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
extension ExpansionRequirementTest where repeat each T == Int {} // expected-error {{same-type requirements between packs and concrete types are not yet supported}}


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

default @MainActor // expected-note {{file-level default isolation previously declared here}}
default nonisolated // expected-error {{invalid redeclaration of file-level default isolation}}

default @Test
// expected-error@-1 {{cannot find type 'Test' in scope}}
// expected-note@-2 {{a file-level default must be '@MainActor', 'nonisolated', '@available', or '@diagnose'}}

default test
// expected-error@-1 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'default'}}

func scopedMissingLParen(b: @_scoped Int) {}
// expected-error@-1:29{{expected arguments for '_scoped' attribute}}

func scopedNotAName(b: @_scoped(0) Int) {}
// expected-error@-1:33{{invalid argument in '_scoped' attribute}}

func scopedKeywordLabel(a: Int, p: @_scoped(default: a) Int) {}
// expected-error@-1:45{{invalid argument in '_scoped' attribute}}

func scopedImmortalAccess(b: @_scoped(&immortal) Int) {}
// expected-error@-1:39{{invalid argument in '_scoped' attribute}}

func scopedTrailingComma(a: Int, b: @_scoped(a,) Int) {}
// expected-error@-1:47{{invalid argument in '_scoped' attribute}}

func scopedLabelWithoutSpecifier(b: @_scoped(left:) Int) {}
// expected-error@-1:51{{expected value in attribute}}
// expected-note@-2:51{{insert value}}
