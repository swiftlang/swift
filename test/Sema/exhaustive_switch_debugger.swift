// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-resilience -enable-nonfrozen-enum-exhaustivity-diagnostics -debugger-support
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-resilience -enable-nonfrozen-enum-exhaustivity-diagnostics -playground

public enum NonExhaustive {
  case a, b
}

public enum NonExhaustivePayload {
  case a(Int), b(Bool)
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive, _ payload: NonExhaustivePayload, flag: Bool) {
  switch value { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}} {{none}} expected-note {{handle unknown values using "@unknown default"}} {{none}}
  case .a: break
  }

  switch value { // no-warning
  case .a: break
  case .b: break
  }
  
  switch value {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch value {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}} {{none}}
  case .a: break
  @unknown case _: break
  }

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.a'}} {{none}} expected-note {{add missing case: '.b'}} {{none}}
  @unknown case _: break
  }

  switch value {
  case _: break
  @unknown case _: break // expected-warning {{case is already handled by previous patterns; consider removing it}}
  }

  // Test being part of other spaces.
  switch value as Optional { // no-warning
  case .a?: break
  case .b?: break
  case nil: break
  }

  switch value as Optional {
  case _?: break
  case nil: break
  } // no-warning

  switch (value, flag) { // no-warning
  case (.a, _): break
  case (.b, false): break
  case (_, true): break
  }

  switch (flag, value) { // no-warning
  case (_, .a): break
  case (false, .b): break
  case (true, _): break
  }

  // Test payloaded enums.
  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{none}} expected-note {{handle unknown values using "@unknown default"}} {{none}}
  case .a: break
  }

  switch payload { // no-warning
  case .a: break
  case .b: break
  }
  
  switch payload {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch payload {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{none}}
  case .a: break
  @unknown case _: break
  }

  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{none}} expected-note {{handle unknown values using "@unknown default"}} {{none}}
  case .a: break
  case .b(false): break
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{none}}
  case .a: break
  case .b(false): break
  @unknown case _: break
  }
}

public func testNonExhaustiveWithinModule(_ value: NonExhaustive, _ payload: NonExhaustivePayload, flag: Bool) {
  switch value { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}}
  case .a: break
  }

  switch value { // no-warning
  case .a: break
  case .b: break
  }
  
  switch value {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch value {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}} {{none}}
  case .a: break
  @unknown case _: break
  }

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.a'}} {{none}} expected-note {{add missing case: '.b'}} {{none}}
  @unknown case _: break
  }

  switch value {
  case _: break
  @unknown case _: break // expected-warning {{case is already handled by previous patterns; consider removing it}}
  }

  // Test being part of other spaces.
  switch value as Optional { // no-warning
  case .a?: break
  case .b?: break
  case nil: break
  }

  switch value as Optional {
  case _?: break
  case nil: break
  } // no-warning

  switch (value, flag) { // no-warning
  case (.a, _): break
  case (.b, false): break
  case (_, true): break
  }

  switch (flag, value) { // no-warning
  case (_, .a): break
  case (false, .b): break
  case (true, _): break
  }

  // Test payloaded enums.
  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{none}}
  case .a: break
  }

  switch payload { // no-warning
  case .a: break
  case .b: break
  }
  
  switch payload {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch payload {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{none}}
  case .a: break
  @unknown case _: break
  }

  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{none}}
  case .a: break
  case .b(false): break
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{none}}
  case .a: break
  case .b(false): break
  @unknown case _: break
  }
}
