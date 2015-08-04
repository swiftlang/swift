// RUN: %target-parse-verify-swift

var stream = ""

// FIXME: We need to address <rdar://22126141> so that the compiler can emit an
// appropriate 'unavailable overload' diagnostic for 'appendNewline'.
print(3, appendNewline: false)
debugPrint(3, appendNewline: false)

// FIXME: Suboptimal diagnostics due to <rdar://22101775>
print(3, &stream) // expected-error {{'&' can only appear immediately in a call argument list}}
debugPrint(3, &stream) // expected-error {{'&' can only appear immediately in a call argument list}}
print(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'print' with an argument list of type}} expected-note {{expected an argument list of type}}
debugPrint(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'debugPrint' with an argument list of type}} expected-note {{expected an argument list of type}}
print(4, quack: 5)
