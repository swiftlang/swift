// RUN: %target-swift-frontend -typecheck %s -verify -package-name myPkg

// MARK: - Correct type eraser

class AnyP: P1 {
  init<T: P1>(erasing t: T) {}
}
@_typeEraser(AnyP) // okay
protocol P1 {}

class AnyP2: P2 {
  init<T: P2>(erasing t: T) {}
}
typealias AnyP2Alias = AnyP2
@_typeEraser(AnyP2Alias)
protocol P2 {}

class AnyCollection<Element> : Collection {
  typealias Element = Element
  init<C: Collection>(erasing c: C) where Element == C.Element {}
}
@_typeEraser(AnyCollection<Self.Element>)
protocol Collection {
  associatedtype Element
}

// MARK: - Parsing Errors

@_typeEraser // expected-error {{expected '(' in '_typeEraser' attribute}}
protocol A1 {}

@_typeEraser() // expected-error {{expected a type name in '@_typeEraser()'}}
protocol A2 {}

@_typeEraser(AnyP // expected-note {{to match this opening '('}}
protocol A3 {} // expected-error {{expected ')' after type name for '@_typeEraser'}}

@_typeEraser(AnyP) // expected-error {{@_typeEraser may only be used on 'protocol' declarations}}
func notAProtocol() {}

// MARK: - Type eraser must be a concrete nominal type

@_typeEraser(Undeclared) // expected-error {{cannot find type 'Undeclared' in scope}}
protocol B1 {}

@_typeEraser((Int, Int)) // expected-error {{type eraser must be a class, struct, or enum}}
protocol B2 {}

protocol InvalidTypeEraser {}
@_typeEraser(InvalidTypeEraser) // expected-error {{type eraser must be a class, struct, or enum}}
protocol B3 {}

class Generic<Param>: B5 { // expected-note {{generic class 'Generic' declared here}}
  init<T: B5>(erasing t: T) {}
}
@_typeEraser(Generic) // expected-error {{reference to generic type 'Generic' requires arguments in <...>}}
protocol B4 {}
@_typeEraser(Generic<Int>) // bound generic is okay
protocol B5 {}

class MoreRestrictive: B6 { // expected-note {{type eraser declared here}}
  init<T: B6>(erasing t: T) {}
}
@_typeEraser(MoreRestrictive) // expected-error {{internal type eraser 'MoreRestrictive' cannot have more restrictive access than protocol 'B6' (which is public)}}
public protocol B6 {}

class MoreRestrictive1: B66 { // expected-note {{type eraser declared here}}
 init<T: B66>(erasing t: T) {}
}
@_typeEraser(MoreRestrictive1) // expected-error {{internal type eraser 'MoreRestrictive1' cannot have more restrictive access than protocol 'B66' (which is package)}}
package protocol B66 {}

package class PkgMoreRestrictive: PB6 { // expected-note {{type eraser declared here}}
 init<T: B6>(erasing t: T) {}
}
@_typeEraser(PkgMoreRestrictive) // expected-error {{package type eraser 'PkgMoreRestrictive' cannot have more restrictive access than protocol 'PB6' (which is public)}}
public protocol PB6 {}

typealias FnAlias = () -> Void
@_typeEraser(FnAlias) // expected-error {{type eraser must be a class, struct, or enum}}
protocol B7 {}

// MARK: - Type eraser must conform to the annotated protocol

class DoesNotConform {} // expected-note {{type eraser declared here}}
@_typeEraser(DoesNotConform) // expected-error {{type eraser 'DoesNotConform' must conform to protocol 'C1'}}
protocol C1 {}

// MARK: - Type eraser must have an initializer in the form init<T>(erasing: T) with T constrained to annotated protocol

class NoArgInit: D1 {} // expected-note {{type eraser declared here}}
@_typeEraser(NoArgInit) // expected-error {{type eraser 'NoArgInit' must have an initializer of the form 'init<T: D1>(erasing: T)'}}
protocol D1 {}

class InvalidArgInit: D2 { // expected-note {{type eraser declared here}}
  init<T>(erasing t: T) {}
}
@_typeEraser(InvalidArgInit) // expected-error {{type eraser 'InvalidArgInit' must have an initializer of the form 'init<T: D2>(erasing: T)'}}
protocol D2 {}

class ExtraArgInit: D3 { // expected-note {{type eraser declared here}}
  init<T: D3>(erasing t: T, extraArg: Int) {}
}
@_typeEraser(ExtraArgInit) // expected-error {{type eraser 'ExtraArgInit' must have an initializer of the form 'init<T: D3>(erasing: T)'}}
protocol D3 {}

class WrongLabelInit: D4 { // expected-note {{type eraser declared here}}
  init<T: D4>(wrongLabel: T) {}
}
@_typeEraser(WrongLabelInit) // expected-error {{type eraser 'WrongLabelInit' must have an initializer of the form 'init<T: D4>(erasing: T)'}}
protocol D4 {}

class NoLabel: D5 { // expected-note {{type eraser declared here}}
  init<T: D5>(_ t: T) {}
}
@_typeEraser(NoLabel) // expected-error {{type eraser 'NoLabel' must have an initializer of the form 'init<T: D5>(erasing: T)'}}
protocol D5 {}

class NonGenericInit<T: D6>: D6 { // expected-note {{type eraser declared here}}
  init(_ t: T) {}
}
@_typeEraser(NonGenericInit<Self>) // expected-error {{type eraser 'NonGenericInit<Self>' must have an initializer of the form 'init<T: D6>(erasing: T)'}}
protocol D6 {}

// MARK: - Unviable initializers

public class UnviableInits: E1 {
  public init<T: E1>(erasing t: T) where T: Hashable {} // expected-note {{'init(erasing:)' cannot have unsatisfied requirements when 'T' = 'some E1'}}
  init<T: E1>(erasing t: T) {} // expected-note {{internal 'init(erasing:)' cannot have more restrictive access than protocol 'E1' (which is public)}}
}
@_typeEraser(UnviableInits) // expected-error {{type eraser 'UnviableInits' has no viable initializer of the form 'init<T: E1>(erasing: T)'}}
public protocol E1 {}

public class UnviableInits11: E11 {
 public init<T: E11>(erasing t: T) where T: Hashable {} // expected-note {{'init(erasing:)' cannot have unsatisfied requirements when 'T' = 'some E11'}}
 package init<T: E11>(erasing t: T) {} // expected-note {{package 'init(erasing:)' cannot have more restrictive access than protocol 'E11' (which is public)}}
}
@_typeEraser(UnviableInits11) // expected-error {{type eraser 'UnviableInits11' has no viable initializer of the form 'init<T: E11>(erasing: T)'}}
public protocol E11 {}

package class PkgUnviableInits: PE1 {
  package init<T: PE1>(erasing t: T) where T: Hashable {} // expected-note {{'init(erasing:)' cannot have unsatisfied requirements when 'T' = 'some PE1'}}
  init<T: PE1>(erasing t: T) {} // expected-note {{internal 'init(erasing:)' cannot have more restrictive access than protocol 'PE1' (which is package)}}
}
@_typeEraser(PkgUnviableInits) // expected-error {{type eraser 'PkgUnviableInits' has no viable initializer of the form 'init<T: PE1>(erasing: T)'}}
package protocol PE1 {}

class FailableInit: E2 {
  init?<T: E2>(erasing t: T) {} // expected-note {{'init(erasing:)' cannot be failable}}
}
@_typeEraser(FailableInit) // expected-error {{type eraser 'FailableInit' has no viable initializer of the form 'init<T: E2>(erasing: T)'}}
protocol E2 {}

// SPI type eraser and non-SPI protocol
@_spi(SPI)
public struct AnyE3_SPI: E3 {
  public init<T: E3>(erasing: T) {} // expected-note {{'init(erasing:)' is SPI, but protocol 'E3' is not}}
}
@_typeEraser(AnyE3_SPI) // expected-error {{type eraser 'AnyE3_SPI' has no viable initializer of the form 'init<T: E3>(erasing: T)'}}
public protocol E3 {}

// SPI type eraser and SPI protocol of different groups
@_spi(SPI2)
public struct AnyE4_SPI: E4 {
  public init<T: E4>(erasing: T) {} // expected-note {{'init(erasing:)' is SPI, but protocol 'E4' is not in the same SPI groups as 'init(erasing:)'}}
}
@_spi(SPI1) @_spi(SPI2)
@_typeEraser(AnyE4_SPI) // expected-error {{type eraser 'AnyE4_SPI' has no viable initializer of the form 'init<T: E4>(erasing: T)'}}
public protocol E4 {}

// Same-group SPI type eraser and protocol
@_spi(SPI1) @_spi(SPI2)
public struct AnyE5_SPI: E5 {
  public init<T: E5>(erasing: T) {}
}
@_spi(SPI2) @_spi(SPI1)
@_typeEraser(AnyE5_SPI) // same SPI groups, okay
public protocol E5 {}
