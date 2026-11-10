// RUN: %target-typecheck-verify-swift

precedencegroup; // expected-error {{expected identifier}}

precedencegroup A; // expected-cparser-error {{expected '{'}} expected-astgen-error {{expected '{}' in precedencegroup}}

precedencegroup B {; // expected-cparser-error {{expected operator attribute identifier}} expected-astgen-error {{expected '}' to end precedencegroup}} expected-astgen-note {{to match this opening '{'}}
