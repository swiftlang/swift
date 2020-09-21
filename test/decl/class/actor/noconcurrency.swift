// RUN: %target-typecheck-verify-swift

actor class C { } // expected-error{{'actor' attribute is only valid when experimental concurrency is enabled}}

