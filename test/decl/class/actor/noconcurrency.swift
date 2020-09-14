// RUN: %target-typecheck-verify-swift

actor class C { } // expected-error{{'actor' classes require experimental concurrency support}}

