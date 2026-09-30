// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip -enable-experimental-feature OnewayNowait

// REQUIRES: concurrency
// REQUIRES: swift_feature_OnewayNowait

// 'nowait' is fire-and-forget: it discards the result and cannot forward errors,
// so it may only be applied to a call to a Void-returning, non-throwing method

actor Greeter {
  func greet() {}
  func value() -> Int { 0 }
  func boom() throws {}
  func boomValue() throws -> Int { 0 }
  var name: String { "greeter" }
}

func test(_ g: Greeter) {
  // Void, non-throwing: OK
  nowait g.greet()

  // Returns a value.
  // expected-error@+1{{'nowait' can only be applied to Void returning calls}}
  nowait g.value()

  // Throwing (Void result)
  // TODO: Drop the secondary thrown-type error of the synthesized operation
  // closure, which duplicates the 'nowait' diagnostic
  // expected-error@+2{{'nowait' cannot be applied to a call that can throw}}
  // expected-error@+1{{thrown expression type 'any Error' cannot be converted to error type 'Never'}}
  nowait g.boom()

  // Throwing and returns a value: the Void check fires first (plus the same
  // secondary thrown-type error).
  // expected-error@+2{{'nowait' can only be applied to Void returning calls}}
  // expected-error@+1{{thrown expression type 'any Error' cannot be converted to error type 'Never'}}
  nowait g.boomValue()

  // Not a function call.
  // expected-error@+1{{'nowait' can only be applied to a function call}}
  nowait g.name
}
