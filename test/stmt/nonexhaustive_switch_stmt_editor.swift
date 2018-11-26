// RUN: %target-typecheck-verify-swift -diagnostics-editor-mode -enable-resilience -enable-nonfrozen-enum-exhaustivity-diagnostics

public enum NonExhaustive {
  case a, b
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive) {
  switch value { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{do you want to add missing cases?}}
  case .a: break
  }

  switch value { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{handle unknown values using "@unknown default"}} {{3-3=@unknown default:\n<#fatalError()#>\n}}
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
}
