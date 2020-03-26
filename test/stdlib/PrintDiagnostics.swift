// RUN: %target-typecheck-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}

print(3, &stream, appendNewline: false) // expected-error {{extra argument 'appendNewline' in call}}
// expected-error@-1:10 {{'&' used with non-inout argument of type 'Any'}}

debugPrint(3, &stream, appendNewline: false) // expected-error {{extra argument 'appendNewline' in call}}
// expected-error@-1:15 {{'&' used with non-inout argument of type 'Any'}}

print(4, quack: 5) // expected-error {{extra argument 'quack' in call}}
