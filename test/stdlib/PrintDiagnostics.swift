// RUN: %target-typecheck-verify-swift

var stream = ""

print(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}
debugPrint(3, &stream) // expected-error{{'&' used with non-inout argument of type 'Any'}}

print(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'print' with an argument list of type '(Int, inout String, appendNewline: Bool)'}}
// expected-note@-1 {{overloads for 'print' exist with these partially matching parameter lists: (Any..., separator: String, terminator: String), (Any..., separator: String, terminator: String, to: inout Target)}}

debugPrint(3, &stream, appendNewline: false) // expected-error {{cannot invoke 'debugPrint' with an argument list of type '(Int, inout String, appendNewline: Bool)'}}
// expected-note@-1 {{verloads for 'debugPrint' exist with these partially matching parameter lists: (Any..., separator: String, terminator: String), (Any..., separator: String, terminator: String, to: inout Target)}}

print(4, quack: 5) // expected-error {{cannot invoke 'print' with an argument list of type '(Int, quack: Int)'}}
// expected-note@-1 {{overloads for 'print' exist with these partially matching parameter lists: (Any..., separator: String, terminator: String), (Any..., separator: String, terminator: String, to: inout Target)}}
