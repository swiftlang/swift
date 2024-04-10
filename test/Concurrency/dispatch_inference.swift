// RUN: %target-swift-frontend  -disable-availability-checking %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -disable-availability-checking %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -disable-availability-checking %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: asserts

import Dispatch

// Tests the inference of @_unsafeSendable and @MainActor when working with
// the Dispatch library, and specifically, DispatchQueue.
@MainActor func onlyOnMainActor() { }

func testMe() {
  DispatchQueue.main.async {
    onlyOnMainActor() // okay, due to inference of @MainActor-ness
  }

  DispatchQueue.main.sync {
    onlyOnMainActor()
  }
}

func testUnsafeSendableInMainAsync() async {
  var x = 5
  DispatchQueue.main.async {
    x = 17 // expected-warning{{mutation of captured var 'x' in concurrently-executing code}}
  }
  print(x)
}

func testUnsafeSendableInAsync(queue: DispatchQueue) async {
  var x = 5
  queue.async {
    x = 17 // expected-warning{{mutation of captured var 'x' in concurrently-executing code}}
  }

  queue.sync {
    x = 17 // okay
  }

  print(x)
}
