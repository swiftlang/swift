// RUN: %target-typecheck-verify-swift

actor C {
  nonisolated func f() { } // expected-error{{'nonisolated' modifier is only valid when experimental concurrency is enabled}}
}



