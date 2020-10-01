// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/redeclaration_other_compat.swift

// We currently allow cross-file redeclarations.
precedencegroup RedeclaredAcrossFiles {}

precedencegroup RedeclaredSameFile {} // expected-note {{previous precedence group declaration here}}
precedencegroup RedeclaredSameFile {} // expected-error {{precedence group redeclared}}

precedencegroup RedeclaredSameFile2 { // expected-note {{previous precedence group declaration here}}
  assignment: true
}
precedencegroup RedeclaredSameFile2 {} // expected-error {{precedence group redeclared}}

// These are all declared in the other file, and so are allowed for now.
infix operator ^^^
prefix operator >>>
postfix operator <<<
infix operator ^^^^

// This is declared as an infix operator in the other file, so no problem.
prefix operator &&&
postfix operator &&&

infix operator %%% // expected-note {{previous operator declaration here}}
infix operator %%% // expected-error {{operator redeclared}}

prefix operator %%% // expected-note {{previous operator declaration here}}
prefix operator %%% // expected-error {{operator redeclared}}

precedencegroup P2 {}
infix operator *** : P2 // expected-note {{previous operator declaration here}}
infix operator *** // expected-error {{operator redeclared}}
