// RUN: %target-swift-emit-sil -sil-verify-all -verify %s

// REQUIRES: concurrency

func foo(_ xs: consuming AsyncStream<Int>) async {
  _ = {
    for await _ in xs {}
  }
}
