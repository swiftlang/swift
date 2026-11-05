// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -swift-version 5

// Unterminated string literals: an unterminated literal runs to the end of the line,
// including the comment with the expectations, and the C++ lexer and SwiftParser
// recover differently, so there is no way to say what both of them produce.
// UNSUPPORTED: parser_astgen

var st_c = "asdfasd    // expected-error {{unterminated string literal}}

func stringliterals(_ d: [String: Int]) {

  // rdar://11385385
  let x = 4
  "Hello \(x+1) world"  // expected-warning {{string literal is unused}}
  
  // expected-error @+1 {{unterminated string literal}}
  "Error: \(x+1"; // expected-error {{cannot find ')' to match opening '(' in string interpolation}}

  // expected-error @+1 {{unterminated string literal}}
  "Error: \(x+1   // expected-error {{cannot find ')' to match opening '(' in string interpolation}}
  ;    // expected-error {{';' statements are not allowed}}

  // rdar://14050788 [DF] String Interpolations can't contain quotes
  "test \("nested")"
  "test \("\("doubly nested")")"
  "test \(d["hi"])"
  "test \("quoted-paren )")"
  "test \("quoted-paren (")"
  "test \("\\")"
  "test \("\n")"
  "test \("\")" // expected-error {{cannot find ')' to match opening '(' in string interpolation}} expected-error {{unterminated string literal}}

  "test \
  // expected-error @-1 {{unterminated string literal}} expected-error @-1 {{invalid escape sequence in literal}}
  "test \("\
  // expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}} expected-error @-1 {{unterminated string literal}}
  "test newline \("something" +
    "something else")"
  // expected-error @-2 {{cannot find ')' to match opening '(' in string interpolation}}
  // expected-error @-2 {{unterminated string literal}} expected-error @-3 {{unterminated string literal}}

  // expected-warning @+2 {{variable 'x2' was never used; consider replacing with '_' or removing it}}
  // expected-error @+1 {{unterminated string literal}}
  var x2 : () = ("hello" + "
  ;
}

func testUnterminatedSingleQuoteStringLiterals() {
  _ = "abc' // expected-error{{unterminated string literal}}
  _ = 'abc" // expected-error{{unterminated string literal}}
}
