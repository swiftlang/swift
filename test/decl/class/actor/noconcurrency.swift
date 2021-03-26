// RUN: %target-typecheck-verify-swift

actor class C { } // expected-warning{{'actor class' has been renamed to 'actor'}}
