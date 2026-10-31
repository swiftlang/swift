// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse -primary-file %s -verify -swift-version 4

class C<throws> {} // expected-cparser-error {{expected an identifier to name generic parameter}} expected-astgen-error {{expected '>' to end generic parameter clause}} expected-astgen-note {{to match this opening '<'}} expected-astgen-error {{unexpected code 'throws>' in class}}
precedencegroup rethrows {} // expected-cparser-error {{expected identifier after 'precedencegroup'}} expected-astgen-error {{expected identifier in precedencegroup}} expected-astgen-error {{unexpected 'rethrows' keyword in precedencegroup}}
