// RUN: %target-typecheck-verify-swift -disable-availability-checking
// ^^^^ notice the, on purpose, missing '-enable-experimental-distributed'
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(macOS 12.0, *)
distributed actor A {}
// expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
