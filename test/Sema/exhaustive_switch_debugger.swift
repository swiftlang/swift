// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution -enable-nonfrozen-enum-exhaustivity-diagnostics -debugger-support
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution -enable-nonfrozen-enum-exhaustivity-diagnostics -playground

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
  switch value { 
  // expected-error@-1 {{switch must be exhaustive}} {{none}} 
  // expected-note@-2 {{add missing case: '.b'}} {{+6:3-3=case .b:\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+6:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+6:3-3=case .b:\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
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

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}} {{+2:3-3=case .b:\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch value { 
  // expected-warning@-1 {{switch must be exhaustive}} {{none}} 
  // expected-note@-2 {{add missing case: '.a'}} {{+5:3-3=case .a:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.b'}} {{+5:3-3=case .b:\n<#code#>\n}}
  // expected-note@-4 {{add missing cases}} {{+5:3-3=case .a:\n<#code#>\ncase .b:\n<#code#>\n}}
  @unknown case _: break
  }

  switch value {
  case _: break
  @unknown case _: break
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
  switch payload { 
  // expected-error@-1 {{switch must be exhaustive}} {{none}} 
  // expected-note@-2 {{add missing case: '.b(_)'}} {{+6:3-3=case .b(_):\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+6:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+6:3-3=case .b(_):\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
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

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{+2:3-3=case .b(_):\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch payload { 
  // expected-error@-1 {{switch must be exhaustive}} {{none}} 
  // expected-note@-2 {{add missing case: '.b(true)'}} {{+7:3-3=case .b(true):\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+7:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+7:3-3=case .b(true):\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
  case .a: break
  case .b(false): break
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{+3:3-3=case .b(true):\n<#code#>\n}}
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

  switch value { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b'}} {{+2:3-3=case .b:\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch value { 
  // expected-warning@-1 {{switch must be exhaustive}} {{none}} 
  // expected-note@-2 {{add missing case: '.a'}} {{+5:3-3=case .a:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.b'}} {{+5:3-3=case .b:\n<#code#>\n}}
  // expected-note@-4 {{add missing cases}} {{+5:3-3=case .a:\n<#code#>\ncase .b:\n<#code#>\n}}
  @unknown case _: break
  }

  switch value {
  case _: break
  @unknown case _: break
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
  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{+2:3-3=case .b(_):\n<#code#>\n}}
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

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(_)'}} {{+2:3-3=case .b(_):\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch payload { // expected-error {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{+3:3-3=case .b(true):\n<#code#>\n}}
  case .a: break
  case .b(false): break
  }

  switch payload { // expected-warning {{switch must be exhaustive}} {{none}} expected-note {{add missing case: '.b(true)'}} {{+3:3-3=case .b(true):\n<#code#>\n}}
  case .a: break
  case .b(false): break
  @unknown case _: break
  }
}
