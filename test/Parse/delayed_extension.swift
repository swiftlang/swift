// RUN: %target-typecheck-verify-swift -swift-version 4

extension X { } // expected-error {{cannot find type 'X' in scope}}
_ = 1
f() // expected-error {{cannot find 'f' in scope}}
