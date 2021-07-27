// RUN: %target-typecheck-verify-swift  -disable-availability-checking %import-libdispatch -warn-concurrency
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
}

func testUnsafeSendableInAsync() async {
  var x = 5
  DispatchQueue.main.async {
    x = 17 // expected-error{{mutation of captured var 'x' in concurrently-executing code}}
  }
  print(x)
}
