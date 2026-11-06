// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-experimental-string-processing
// RUN: %target-typecheck-verify-swift -disable-experimental-string-processing -enable-bare-slash-regex
// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing -disable-experimental-string-processing -enable-bare-slash-regex

prefix operator /

_ = /x/
// expected-cparser-error@-1 {{'/' is not a prefix unary operator}}
// expected-cparser-error@-2 {{cannot find 'x' in scope}}
// expected-cparser-error@-3 {{'/' is not a postfix unary operator}} expected-astgen-error@-3 {{missing 'Regex' declaration, probably because the '_StringProcessing' module was not imported properly}}

_ = #/x/# // expected-cparser-error {{expected a macro identifier}} expected-astgen-error {{missing 'Regex' declaration, probably because the '_StringProcessing' module was not imported properly}}

func foo(_ x: Regex<Substring>) {} // expected-error {{cannot find type 'Regex' in scope}}
