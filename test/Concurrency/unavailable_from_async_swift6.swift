// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6

// REQUIRES: concurrency
// REQUIRES: asserts

struct API {
  @available(*, noasync, message: "use complete() instead")
  func wait() {}

  @preconcurrency
  @available(*, noasync, message: "use complete() instead")
  func waitUntilComplete() {}

  func complete() async {}
}

func test(v: API) async {
  v.wait() // expected-error {{instance method 'wait' is unavailable from asynchronous contexts; use complete() instead}}
  v.waitUntilComplete() // expected-warning {{instance method 'waitUntilComplete' is unavailable from asynchronous contexts; use complete() instead}}
}
