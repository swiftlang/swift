// RUN: %target-typecheck-verify-swift -swift-version 4

extension X { } // expected-error {{use of undeclared type 'X'}}
_ = 1
f() // expected-error {{use of unresolved identifier 'f'}}
