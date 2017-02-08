// RUN: %target-typecheck-verify-swift

var êx = "" // expected-error{{invalid UTF-8 found in source file}} {{5-6= }} 

// Make sure we don't stop processing the whole file.
static func foo() {} // expected-error{{static methods may only be declared on a type}} {{1-8=}}

// UTF-8 RFC 2279: The octet values FE and FF never appear.
// UTF-8 RFC 3629: The octet values C0, C1, F5 to FF never appear.
// Below this line are such octets that should be skipped by the lexer.
// They may not be rendered correctly by your text editor, if at all.

// Begin magic UTF-8 garbage
// 0xC0
¿ // expected-error {{invalid UTF-8 found in source file}}
// 0xC1
¡ // expected-error {{invalid UTF-8 found in source file}}
// 0xF5
ı // expected-error {{invalid UTF-8 found in source file}}
// 0xF6
ˆ // expected-error {{invalid UTF-8 found in source file}}
// 0xF7
˜ // expected-error {{invalid UTF-8 found in source file}}
// 0xF8
¯ // expected-error {{invalid UTF-8 found in source file}}
// 0xF9
˘ // expected-error {{invalid UTF-8 found in source file}}
// 0xFB
˚ // expected-error {{invalid UTF-8 found in source file}}
// 0xFC
¸ // expected-error {{invalid UTF-8 found in source file}}
// 0xFD
˝ // expected-error {{invalid UTF-8 found in source file}}
// End magic UTF-8 garbage

// Make sure we don't stop processing the whole file.
static func bar() {} // expected-error{{static methods may only be declared on a type}} {{1-8=}}
