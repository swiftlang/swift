// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %import-libdispatch -strict-concurrency=complete %s -emit-sil -o /dev/null -verify -verify-additional-prefix tns-

// REQUIRES: concurrency
// REQUIRES: libdispatch

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
    // expected-tns-warning @-1 {{sending 'x' risks causing data races}}
    // expected-tns-note @-2 {{'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
  print(x) // expected-tns-note {{access can happen concurrently}}
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
