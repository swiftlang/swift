// RUN: %target-typecheck-verify-swift -package-name myPkg


// Protocol requirement is witnessed from a member of a
// less-visible extension
public protocol P {
  func publicRequirement()
}

package protocol Pkg: P {
  func packageRequirement()
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
  // expected-note@-1 {{protocol requires function 'privateRequirementCannotWork()' with type '() -> ()'}}
}

extension S {
  public func publicRequirement() {} // expected-note {{mark the instance method as 'public' to satisfy the requirement}}
  internal func internalRequirement() {} // expected-note {{mark the instance method as 'internal' to satisfy the requirement}}
  fileprivate func fileprivateRequirement() {}
  fileprivate func privateRequirement() {}

  // Cannot witness requirement in another protocol!
  private func privateRequirementCannotWork() {}
}

public struct T : S {}
// expected-error@-1 {{type 'T' does not conform to protocol 'S'}}
// expected-note@-2 {{add stubs for conformance}}
// expected-warning@-3 {{method 'internalRequirement()' must be as accessible as its enclosing type because it matches a requirement in protocol 'Q'}}
// expected-warning@-4 {{method 'publicRequirement()' must be as accessible as its enclosing type because it matches a requirement in protocol 'P'}}

protocol Qpkg : Pkg {
  func internalRequirement()
}

fileprivate protocol Rpkg : Qpkg {
  func fileprivateRequirement()
}
private protocol Spkg : Rpkg {
  func privateRequirement()
  func privateRequirementCannotWork()
  // expected-note@-1 {{protocol requires function 'privateRequirementCannotWork()' with type '() -> ()'}}
}

extension Spkg {
  public func publicRequirement() {} // expected-note {{mark the instance method as 'public' to satisfy the requirement}}
  package func packageRequirement() {} // expected-note {{mark the instance method as 'package' to satisfy the requirement}}
  internal func internalRequirement() {} // expected-note {{mark the instance method as 'internal' to satisfy the requirement}}
  fileprivate func fileprivateRequirement() {}
  fileprivate func privateRequirement() {}

  // Cannot witness requirement in another protocol!
  private func privateRequirementCannotWork() {}
}

public struct Tpkg : Spkg {}
// expected-error@-1 {{type 'Tpkg' does not conform to protocol 'Spkg'}}
// expected-note@-2 {{add stubs for conformance}}
// expected-warning@-3 {{method 'internalRequirement()' must be as accessible as its enclosing type because it matches a requirement in protocol 'Qpkg'}}
// expected-warning@-4 {{method 'packageRequirement()' must be as accessible as its enclosing type because it matches a requirement in protocol 'Pkg'}}
// expected-warning@-5 {{method 'publicRequirement()' must be as accessible as its enclosing type because it matches a requirement in protocol 'P'}}

// This is also OK
@usableFromInline
internal protocol U : P {}

extension U {
  public func publicRequirement() {}
}

public struct SS : U {}

@usableFromInline
package protocol Upkg : P {}

extension Upkg {
  public func publicRequirement() {}
}

public struct SSpkg : Upkg {}


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

package protocol Q2pkg : P2 {}

extension Q2pkg {
  // note: not public
  func publicRequirement() {}
  // expected-note@-1 {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
}

public struct T2pkg : Q2pkg {} // expected-error {{method 'publicRequirement()' must be declared public because it matches a requirement in public protocol 'P2'}}

public struct Foo {
  public init(value: Int) {}
}
public protocol PublicProtocol {
  init?(integer: Int)
}
protocol InternalProtocol: PublicProtocol {}
extension InternalProtocol {
  public init(integer: Int) {} // expected-note {{mark the initializer as 'public' to satisfy the requirement}}
}
extension Foo: PublicProtocol, InternalProtocol {} // expected-warning {{initializer 'init(integer:)' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProtocol'}}
