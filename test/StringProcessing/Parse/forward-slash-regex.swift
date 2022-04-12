// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex

prefix operator /  // expected-error {{prefix operator may not contain '/'}}
prefix operator ^/ // expected-error {{prefix operator may not contain '/'}}
prefix operator /^/ // expected-error {{prefix operator may not contain '/'}}
