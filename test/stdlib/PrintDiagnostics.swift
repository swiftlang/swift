// RUN: %target-typecheck-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
print(3, &stream, appendNewline: false) // expected-error {{incorrect argument label in call (have '_:_:appendNewline:', expected '_:_:separator:')}}
debugPrint(3, &stream, appendNewline: false) // expected-error {{incorrect argument label in call (have '_:_:appendNewline:', expected '_:_:separator:')}}
print(4, quack: 5) // expected-error {{incorrect argument label in call (have '_:quack:', expected '_:separator:')}}
