// RUN: %target-parse-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
print(4, quack: 5) // expected-error {{'print' is unavailable: Please wrap your tuple argument in parentheses: 'print((...))'}}
debugPrint(4, quack: 5) // expected-error {{'debugPrint' is unavailable: Please wrap your tuple argument in parentheses: 'debugPrint((...))'}}
