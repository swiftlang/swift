// RUN: %target-parse-verify-swift

var stream = ""

print(3, appendNewline: false) // expected-error {{'print(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'print((...), terminator: "")'}}
debugPrint(3, appendNewline: false) // expected-error {{'debugPrint(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'debugPrint((...), terminator: "")'}}

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
print(3, &stream, appendNewline: false) // expected-error {{cannot pass immutable value as inout argument: implicit conversion from 'String' to 'OutputStreamType' requires a temporary}}
debugPrint(3, &stream, appendNewline: false) // expected-error {{cannot pass immutable value as inout argument: implicit conversion from 'String' to 'OutputStreamType' requires a temporary}} 
print(4, quack: 5) // expected-error {{argument labels '(_:, quack:)' do not match any available overloads}}
// expected-note@-1{{overloads for 'print' exist with these partially matching parameter lists: (Any..., separator: String, terminator: String), (T, appendNewline: Bool), (T, inout OutputStreamType), (T, inout OutputStreamType, appendNewline: Bool)}}
