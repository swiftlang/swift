// RUN: %target-typecheck-verify-swift

actor class C { } // expected-error{{'actor' modifier is only valid when experimental concurrency is enabled}}

