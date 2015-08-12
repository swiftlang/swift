// RUN: %target-parse-verify-swift

var stream = ""

// FIXME: We need to address <rdar://22126141> so that the compiler can emit an
// appropriate 'unavailable overload' diagnostic for 'appendNewline'.
print(3, appendNewline: false) // expected-error {{'print(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'print((...), terminator: "")'}}
debugPrint(3, appendNewline: false) // expected-error {{'debugPrint(_:appendNewline:)' is unavailable: Please use 'terminator: ""' instead of 'appendNewline: false': 'debugPrint((...), terminator: "")'}}

// FIXME: Suboptimal diagnostics due to <rdar://22101775>
print(3, &stream) // expected-error {{'&' can only appear immediately in a call argument list}}
debugPrint(3, &stream) // expected-error {{'&' can only appear immediately in a call argument list}}
print(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'print' with an argument list of type}} expected-note {{expected an argument list of type}}
debugPrint(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'debugPrint' with an argument list of type}} expected-note {{expected an argument list of type}}
print(4, quack: 5) // expected-error {{cannot invoke 'print' with an argument list of type '(Int, quack: Int)'}} expected-note {{expected an argument list of type '(Any..., separator: String, terminator: String)'}}
