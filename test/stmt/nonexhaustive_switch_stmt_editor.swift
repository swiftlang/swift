// RUN: %target-typecheck-verify-swift -enable-library-evolution -enable-nonfrozen-enum-exhaustivity-diagnostics

public enum NonExhaustive {
  case a, b
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive) {
  switch value { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '.b'}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}}
  // expected-note@-3 {{add missing cases}}
  case .a: break
  }

  // expected-warning@+2 {{switch covers known cases, but 'NonExhaustive' may have additional unknown values}}
  // expected-note@+1 {{handle unknown values using "@unknown default"}} {{+3:3-3=@unknown default:\n<#fatalError()#>\n}}
  switch value {
  case .a: break
  case .b: break
  }

  switch value {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.a'}} {{+6:3-3=case .a:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.b'}} {{+6:3-3=case .b:\n<#code#>\n}}
  // expected-note@-4 {{handle unknown values using "@unknown default"}} {{+6:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-5 {{add missing cases}} {{+6:3-3=case .a:\n<#code#>\ncase .b:\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
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
