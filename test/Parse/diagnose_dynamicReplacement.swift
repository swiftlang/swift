// RUN: %target-typecheck-verify-swift -swift-version 5

dynamic func dynamic_replaceable() {

}

@_dynamicReplacement
// expected-cparser-error@-1 {{expected '(' in '_dynamicReplacement' attribute}} expected-astgen-error@-1 {{expected '(', @_dynamicReplacement argument, and ')' in attribute}}
func test_dynamic_replacement_for() {
}

@_dynamicReplacement( // expected-astgen-note {{to match this opening '('}} expected-astgen-error {{expected ')' to end attribute}}
// expected-cparser-error@-1 {{expected 'for' in '_dynamicReplacement' attribute}} expected-astgen-error@-1 {{expected argument for '@_dynamicReplacement' attribute}}
func test_dynamic_replacement_for2() {
}

@_dynamicReplacement(for: dynamically_replaceable() // expected-note {{to match this opening '('}} expected-astgen-error {{replaced function 'dynamically_replaceable()' could not be found}} expected-astgen-error {{expected ')' to end attribute}}
func test_dynamic_replacement_for3() {
// expected-cparser-error@-1 {{expected ')' after function name for '@_dynamicReplacement'}}
}
