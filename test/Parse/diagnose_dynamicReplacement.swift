// RUN: %target-typecheck-verify-swift -swift-version 5

dynamic func dynamic_replaceable() {

}

@_dynamicReplacement
// expected-error@-1 {{expected '(' in '_dynamicReplacement' attribute}}
func test_dynamic_replacement_for() {
}

@_dynamicReplacement(
// expected-error@-1 {{expected 'for' in '_dynamicReplacement' attribute}}
func test_dynamic_replacement_for2() {
}

@_dynamicReplacement(for: dynamically_replaceable() // expected-note {{to match this opening '('}}
func test_dynamic_replacement_for3() {
// expected-error@-1 {{expected ')' after function name for @_dynamicReplacement}}
}
