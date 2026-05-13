// RUN: %target-swift-frontend -emit-sil -swift-version 6 -verify %s
// REQUIRES: asserts

// Crash in SendNonSendable: "Require PartitionOp's argument should
// already be tracked" when a @convention(thin) closure captures a local.

func f() {
  let x = 0
  let g: @convention(thin) () -> Void = { _ = x } // expected-error {{nontrivial thin function reference}}
  _ = g
}
