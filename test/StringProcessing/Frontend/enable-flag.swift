// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -enable-experimental-string-processing
// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing -enable-bare-slash-regex
// RUN: %target-typecheck-verify-swift -disable-experimental-string-processing -enable-experimental-string-processing -enable-bare-slash-regex

// REQUIRES: swift_in_compiler

prefix operator / // expected-error {{prefix operator may not contain '/'}}

_ = /x/
_ = #/x/#

func foo(_ x: Regex<Substring>) {}
