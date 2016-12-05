// RUN: %target-typecheck-verify-swift

precedencegroup; // expected-error {{expected identifier}}

precedencegroup A; // expected-error {{expected '{'}}

precedencegroup B {; // expected-error {{expected operator attribute identifier}}
