// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -parse -primary-file %t/swift3.swift -verify -swift-version 3
// RUN: %target-swift-frontend -parse -primary-file %t/swift4.swift -verify -swift-version 4

// 'throws' or 'rethrows' are allowed as an identifier in Swift 3.

// BEGIN swift3.swift
class C<throws> {}
precedencegroup rethrows {}

// BEGIN swift4.swift
class C<throws> {} // expected-error {{expected an identifier to name generic parameter}}
precedencegroup rethrows {} // expected-error {{expected identifier after 'precedencegroup'}}
