// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: OS=macosx

do {
  struct Test {
    init<T: Sendable>(value: T) {}
    init<T>(value: T) {}
  }

  struct SendableOnly {
    init<T: Sendable>(value: T) {}
  }

  func testNonSendable<T>(v: T) { // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
    _ = Test(value: v) // Ok (non-Sendable overload)
    _ = SendableOnly(value: v)
    // expected-warning@-1 {{type 'T' does not conform to the 'Sendable' protocol}}
  }

  func testSendable<T: Sendable>(v: T) {
    _ = Test(value: v) // Ok
    _ = SendableOnly(value: v) // Ok
  }
}

do {
  class K {
    func value() {}
  }

  struct X {
    var fn: (Int) -> K
    func fn(_: Int) -> Int { 42 }
  }

  func sendable<T>(_ fn: (Int) -> T) -> T { fn(42) }
  func sendable<T: Sendable>(_ fn: (Int) -> T) -> T { fn(0) }

  func test(x: X) {
    let res = sendable(x.fn) // Ok (non-ambiguous and non-Sendable overload)
    res.value() // To make sure that previous expression picks a property
    let _: K = sendable(x.fn) // Ok (picks `sendable<T>` with a property)
    let _: Int = sendable(x.fn) // Ok (picks `sendable<T: Sendable>` with a method)
  }
}
