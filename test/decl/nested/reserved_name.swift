// RUN: %target-typecheck-verify-swift

struct S1 {
  // expected-error @+4 {{type member may not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-error @+3 {{type member may not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-note @+2 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Type`}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Type`}}
  enum Type {
    case A
  }
}

struct S2 {
  enum `Type` {
    case A
  }
}

let s1: S1.Type = .A // expected-error{{type of expression is ambiguous without more context}}
let s2: S2.`Type` = .A // no-error
