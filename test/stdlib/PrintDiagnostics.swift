// RUN: %target-typecheck-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
print(3, &stream, appendNewline: false) // expected-error {{cannot pass immutable value as inout argument: implicit conversion from 'String' to 'TextOutputStream' requires a temporary}}
debugPrint(3, &stream, appendNewline: false) // expected-error {{cannot pass immutable value as inout argument: implicit conversion from 'String' to 'TextOutputStream' requires a temporary}}
print(4, quack: 5) // expected-error {{argument labels '(_:, quack:)' do not match any available overloads}}
// expected-note@-1{{overloads for 'print' exist with these partially matching parameter lists: (Any..., separator: String, terminator: String), (T, appendNewline: Bool), (T, inout TextOutputStream), (T, inout TextOutputStream, appendNewline: Bool)}}
