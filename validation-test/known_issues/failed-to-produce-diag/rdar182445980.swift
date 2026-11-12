// rdar://182445980
// RUN: %target-typecheck-verify-swift

protocol Trait {}
protocol TestTrait: Trait {}
struct ConditionTrait: TestTrait {}

extension Trait where Self == ConditionTrait {
  static func disabled(_ comment: String? = nil) -> Self { ConditionTrait() }
}

@attached(peer)
macro Test(_ traits: any TestTrait...) = #externalMacro(module: "TestingMacros", type: "TestDeclarationMacro")
// expected-warning@-1 {{external macro implementation type 'TestingMacros.TestDeclarationMacro' could not be found for macro 'Test'; plugin for module 'TestingMacros' not found}}

@Test(.disabled) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
func test() {}

func takesTraits(_ traits: any TestTrait...) {}
func plainCall() {
  takesTraits(.disabled) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
