// rdar://179397892
// RUN: %target-typecheck-verify-swift

protocol Trait {}
protocol TestTrait: Trait {}
struct ConditionTrait: TestTrait {}
struct Bug: TestTrait {}

extension Trait where Self == ConditionTrait {
  static func disabled(if condition: Bool) -> Self { ConditionTrait() }
}

extension Trait where Self == Bug {
  static func bug(_ url: String) -> Self { Bug() }
  static func bug(id: Int) -> Self { Bug() }
}

@attached(peer)
macro Test(_ displayName: String? = nil, _ traits: any TestTrait...) = #externalMacro(module: "TestingMacros", type: "TestDeclarationMacro")
// expected-warning@-1 {{external macro implementation type 'TestingMacros.TestDeclarationMacro' could not be found for macro 'Test'; plugin for module 'TestingMacros' not found}}

let isWinter = false

@Test("Tomato", // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      .disabled(if: isWinter)
      .bug("rdar://123456789"))
func tomato() {}

func takesTraits(_ traits: any TestTrait...) {}
func plainCall() {
  takesTraits(.disabled(if: isWinter) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
              .bug("rdar://123456789"))
}
