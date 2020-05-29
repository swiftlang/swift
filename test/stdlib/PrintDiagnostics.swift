// RUN: %target-typecheck-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}

print(3, &stream, appendNewline: false) // expected-error {{incorrect argument label in call (have '_:_:appendNewline:', expected '_:_:separator:')}}
// expected-error@-1:10 {{'&' used with non-inout argument of type 'Any'}}
// expected-error@-2:34 {{cannot convert value of type 'Bool' to expected argument type 'String'}}

debugPrint(3, &stream, appendNewline: false) // expected-error {{incorrect argument label in call (have '_:_:appendNewline:', expected '_:_:separator:')}}
// expected-error@-1:15 {{'&' used with non-inout argument of type 'Any'}}
// expected-error@-2:39 {{cannot convert value of type 'Bool' to expected argument type 'String'}}

print(4, quack: 5) // expected-error {{incorrect argument label in call (have '_:quack:', expected '_:separator:')}}
// expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'String'}}
