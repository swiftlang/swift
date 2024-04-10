/// SPI variant of implementation-only-inlinable-conformances with the "Bad"
/// declarations defined as local SPI. Also check that SPI conformances
/// can be used within inlinable SPI decls.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-public-helper.swift

// RUN: %target-typecheck-verify-swift -I %t

import NormalLibrary

@_spi(X)
extension NormalStruct: @retroactive NormalProto {
  public typealias Assoc = Int
}
@_spi(X)
extension GenericStruct: @retroactive NormalProto {
  public typealias Assoc = Int
}
@_spi(X)
extension NormalClass: @retroactive NormalProto {
  public typealias Assoc = Int
}

@_spi(X)
public struct BadStruct {}
@_spi(X)
public protocol BadProto {}
@_spi(X)
open class BadClass {}

@_spi(X)
public struct IntLike: ExpressibleByIntegerLiteral, Equatable {
  public init(integerLiteral: Int) {}
}

@available(*, unavailable)
public typealias X = Int

public typealias NormalProtoAssoc<T: NormalProto> = T.Assoc
@inlinable func testConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  _ = x
  _ = NormalProtoAssoc<NormalStruct>() // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil
  _ = x
  _ = NormalProtoAssoc<NormalStruct>()
}

func internalConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil // okay
  _ = x
  _ = NormalProtoAssoc<NormalStruct>() // okay
}

public struct NormalProtoAssocHolder<T: NormalProto> {
  public var value: T.Assoc?
  public init() {}
  public init(_ value: T?) {}
}
@inlinable func testConformanceInBoundGeneric() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the implicit init.
  _ = NormalProtoAssocHolder<NormalStruct>() // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // expected-error 2{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestConformanceInBoundGeneric() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil
  _ = x
  _ = NormalProtoAssocHolder<NormalStruct>()
  _ = NormalProtoAssocHolder(nil as NormalStruct?)
}

func internalConformanceInBoundGeneric() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // okay
  _ = x
  _ = NormalProtoAssocHolder<NormalStruct>() // okay
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // okay
}

@inlinable func testDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  let alias = x is NormalProtoAssoc<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  return normal || alias
}

@_spi(AcceptInSPI)
@inlinable public func SPItestDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct>
  let alias = x is NormalProtoAssoc<NormalStruct>
  return normal || alias
}

func internalDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // okay
  let alias = x is NormalProtoAssoc<NormalStruct> // okay
  return normal || alias
}

@inlinable func testSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
    break
  default:
    break
  }
}

@_spi(AcceptInSPI)
@inlinable public func SPItestSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>:
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>:
    break
  default:
    break
  }
}

func internalSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>: // okay
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>: // okay
    break
  default:
    break
  }
}

public enum NormalProtoEnumUser<T: NormalProto> {
  case a
}

@inlinable func testEnum() {
  // FIXME: We get this error twice: once for the pattern and once for the implicit TypeExpr.
  let x: NormalProtoEnumUser<NormalStruct> = .a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the case.
  _ = NormalProtoEnumUser<NormalStruct>.a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestEnum() {
  let x: NormalProtoEnumUser<NormalStruct> = .a
  _ = x
  _ = NormalProtoEnumUser<NormalStruct>.a
}

func internalEnum() {
  let x: NormalProtoEnumUser<NormalStruct> = .a // okay
  _ = x
  _ = NormalProtoEnumUser<NormalStruct>.a // okay
}

@usableFromInline func testFuncImpl<T: NormalProto>(_: T.Type) {}

@inlinable func testFunc() {
  testFuncImpl(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestFunc() {
  testFuncImpl(NormalStruct.self)
}

func internalFunc() {
  testFuncImpl(NormalStruct.self) // okay
}

public struct ForTestingMembers {
  public init() {}
  public init<T: NormalProto>(_: T.Type) {}

  public subscript<T: NormalProto>(_: T.Type) -> Int {
    get { return 0 }
    set {}
  }

  public func method<T: NormalProto>(_: T.Type) {}
}

@inlinable func testMembers() {
  _ = ForTestingMembers(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  _ = ForTestingMembers.init(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}

  _ = ForTestingMembers()[NormalStruct.self] // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  var instance = ForTestingMembers()
  instance[NormalStruct.self] = 1 // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}

  ForTestingMembers().method(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestMembers() {
  _ = ForTestingMembers(NormalStruct.self)
  _ = ForTestingMembers.init(NormalStruct.self)

  _ = ForTestingMembers()[NormalStruct.self]
  var instance = ForTestingMembers()
  instance[NormalStruct.self] = 1

  ForTestingMembers().method(NormalStruct.self)
}

extension NormalProtoAssocHolder {
  public static func testAnotherConformance<U: NormalProto>(_: U.Type) {}
}

@inlinable func testMultipleConformances() {
  NormalProtoAssocHolder<NormalStruct>.testAnotherConformance(NormalClass.self)
 // expected-error@-1 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  // expected-error@-2 {{cannot use conformance of 'NormalClass' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestMultipleConformances() {
  NormalProtoAssocHolder<NormalStruct>.testAnotherConformance(NormalClass.self)
}

@inlinable func localTypeAlias() {
  typealias LocalUser = NormalProtoAssocHolder<NormalStruct> // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  typealias LocalGenericUser<T> = (T, NormalProtoAssocHolder<NormalStruct>) // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}

  typealias LocalProtoAssoc<T: NormalProto> = T.Assoc
  _ = LocalProtoAssoc<NormalStruct>() // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPIlocalTypeAlias() {
  typealias LocalUser = NormalProtoAssocHolder<NormalStruct>
  typealias LocalGenericUser<T> = (T, NormalProtoAssocHolder<NormalStruct>)

  typealias LocalProtoAssoc<T: NormalProto> = T.Assoc
  _ = LocalProtoAssoc<NormalStruct>()
}

@inlinable func localFunctions() {
  func local(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  func localReturn() -> NormalProtoAssocHolder<NormalStruct> { fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  let _ = { (_: NormalProtoAssocHolder<NormalStruct>) in return } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
  let _ = { () -> NormalProtoAssocHolder<NormalStruct> in fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPIlocalFunctions() {
  func local(_: NormalProtoAssocHolder<NormalStruct>) {}
  func localReturn() -> NormalProtoAssocHolder<NormalStruct> { fatalError() }
  let _ = { (_: NormalProtoAssocHolder<NormalStruct>) in return }
  let _ = { () -> NormalProtoAssocHolder<NormalStruct> in fatalError() }
}

@inlinable public func signatureOfInlinable(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}

public func testDefaultArgument(_: Int = NormalProtoAssoc<NormalStruct>()) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; the conformance is declared as SPI}}

@_spi(AcceptInSPI)
@inlinable public func SPIsignatureOfInlinable(_: NormalProtoAssocHolder<NormalStruct>) {}

@_spi(AcceptInSPI)
public func SPItestDefaultArgument(_: Int = NormalProtoAssoc<NormalStruct>()) {}

public class SubclassOfNormalClass: NormalClass {}

@inlinable public func testInheritedConformance() {
  _ = NormalProtoAssocHolder<SubclassOfNormalClass>.self // expected-error {{cannot use conformance of 'NormalClass' to 'NormalProto' here; the conformance is declared as SPI}}
}
@inlinable public func testSpecializedConformance() {
  _ = NormalProtoAssocHolder<GenericStruct<Int>>.self // expected-error {{cannot use conformance of 'GenericStruct<T>' to 'NormalProto' here; the conformance is declared as SPI}}
}

@_spi(AcceptInSPI)
@inlinable public func SPItestInheritedConformance() {
  _ = NormalProtoAssocHolder<SubclassOfNormalClass>.self
}
@_spi(AcceptInSPI)
@inlinable public func SPItestSpecializedConformance() {
  _ = NormalProtoAssocHolder<GenericStruct<Int>>.self
}
