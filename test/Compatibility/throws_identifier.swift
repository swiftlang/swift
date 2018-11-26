// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse -primary-file %s -verify -swift-version 4

class C<throws> {} // expected-error {{expected an identifier to name generic parameter}}
precedencegroup rethrows {} // expected-error {{expected identifier after 'precedencegroup'}}
