// RUN: %target-typecheck-verify-swift


// Protocol requirement is witnessed from a member of a
// less-visible extension
public protocol P {
  func publicRequirement()
}

protocol Q : P {
  func internalRequirement()
}

fileprivate protocol R : Q {
  func fileprivateRequirement()
}

private protocol S : R {
  func privateRequirement()
  func privateRequirementCannotWork()
  // expected-note@-1 {{protocol requires function 'privateRequirementCannotWork()' with type '() -> ()'; do you want to add a stub?}}
}

extension S {
  public func publicRequirement() {}
  internal func internalRequirement() {}
  fileprivate func fileprivateRequirement() {}
  fileprivate func privateRequirement() {}

  // Cannot witness requirement in another protocol!
  private func privateRequirementCannotWork() {}
}


public struct T : S {}
// expected-error@-1 {{type 'T' does not conform to protocol 'S'}}

// This is also OK
@usableFromInline
internal protocol U : P {}

extension U {
  public func publicRequirement() {}
}

public struct SS : U {}

// Currently this is banned
public protocol P2 {
  func publicRequirement()
}

protocol Q2 : P2 {}

extension Q2 {
  // note: not public
  func publicRequirement() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}

public struct T2 : Q2 {} // expected-error {{method 'publicRequirement()' must be declared public because it matches a requirement in public protocol 'P2'}}
