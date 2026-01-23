// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule \
// RUN:   %S/Inputs/implementation-only-import-in-decls-public-helper.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule \
// RUN:   %S/Inputs/implementation-only-import-in-decls-helper.swift -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

/// Access-level on imports allow references from implicitly inlinable code.
// RUN: %target-typecheck-verify-swift -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -verify-additional-prefix access-level-

/// @_implementationOnly rejects references from implicitly inlinable code.
// RUN: %target-typecheck-verify-swift -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:    -D IOI -verify-additional-prefix ioi-

// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_cross_compiling

#if IOI
@_implementationOnly import BADLibrary // expected-ioi-warning {{safely use '@_implementationOnly' without library evolution by setting '-enable-experimental-feature CheckImplementationOnly' for 'main'}}
#else
internal import BADLibrary // expected-access-level-note 35 {{imported as 'internal' from 'BADLibrary' here}}
#endif
import NormalLibrary

public typealias NormalProtoAssoc<T: NormalProto> = T.Assoc
@inlinable func testConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  _ = x
  _ = NormalProtoAssoc<NormalStruct>() // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

func internalConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}}
  _ = x
  _ = NormalProtoAssoc<NormalStruct>() // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}}
}

@export(interface)
func internalConformanceInTypealiasNEIC() {
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
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the implicit init.
  _ = NormalProtoAssocHolder<NormalStruct>() // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // expected-error 2{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

func internalConformanceInBoundGeneric() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}}
  _ = x
  _ = NormalProtoAssocHolder<NormalStruct>() // expected-ioi-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}}
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // expected-ioi-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}}
}

@export(interface)
func internalConformanceInBoundGenericNEIC() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // okay
  _ = x
  _ = NormalProtoAssocHolder<NormalStruct>() // okay
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // okay
}

@inlinable func testDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  let alias = x is NormalProtoAssoc<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  return normal || alias
}

func internalDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
  let alias = x is NormalProtoAssoc<NormalStruct> // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
  return normal || alias
}

@export(interface)
func internalDowncastNEIC(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // okay
  let alias = x is NormalProtoAssoc<NormalStruct> // okay
  return normal || alias
}

@inlinable func testSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
    break
  default:
    break
  }
}

func internalSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>: // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>: // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
    break
  default:
    break
  }
}

@export(interface)
func internalSwitchNEIC(_ x: Any) {
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
  let x: NormalProtoEnumUser<NormalStruct> = .a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the case.
  _ = NormalProtoEnumUser<NormalStruct>.a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

func internalEnum() {
  let x: NormalProtoEnumUser<NormalStruct> = .a // expected-ioi-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
  _ = x
  _ = NormalProtoEnumUser<NormalStruct>.a // expected-ioi-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
}

@export(interface)
func internalEnumNEIC() {
  let x: NormalProtoEnumUser<NormalStruct> = .a // okay
  _ = x
  _ = NormalProtoEnumUser<NormalStruct>.a // okay
}

@usableFromInline func testFuncImpl<T: NormalProto>(_: T.Type) {}

@inlinable func testFunc() {
  testFuncImpl(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

func internalFunc() {
  testFuncImpl(NormalStruct.self)  // expected-ioi-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as implementation-only}}
}

@export(interface)
func internalFuncNEIC() {
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
  _ = ForTestingMembers(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  _ = ForTestingMembers.init(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}

  _ = ForTestingMembers()[NormalStruct.self] // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  var instance = ForTestingMembers()
  instance[NormalStruct.self] = 1 // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}

  ForTestingMembers().method(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

extension NormalProtoAssocHolder {
  public static func testAnotherConformance<U: NormalProto>(_: U.Type) {}
}

@inlinable func testMultipleConformances() {
  NormalProtoAssocHolder<NormalStruct>.testAnotherConformance(NormalClass.self)
 // expected-error@-1 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  // expected-error@-2 {{cannot use conformance of 'NormalClass' to 'NormalProto' here; 'BADLibrary'}}
}

@inlinable func localTypeAlias() {
  typealias LocalUser = NormalProtoAssocHolder<NormalStruct> // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  typealias LocalGenericUser<T> = (T, NormalProtoAssocHolder<NormalStruct>) // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}

  typealias LocalProtoAssoc<T: NormalProto> = T.Assoc
  _ = LocalProtoAssoc<NormalStruct>() // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

@inlinable func localFunctions() {
  func local(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  func localReturn() -> NormalProtoAssocHolder<NormalStruct> { fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  let _ = { (_: NormalProtoAssocHolder<NormalStruct>) in return } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
  let _ = { () -> NormalProtoAssocHolder<NormalStruct> in fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}
}

@inlinable public func signatureOfInlinable(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}

public func testDefaultArgument(_: Int = NormalProtoAssoc<NormalStruct>()) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary'}}


public class SubclassOfNormalClass: NormalClass {}

@inlinable public func testInheritedConformance() {
  _ = NormalProtoAssocHolder<SubclassOfNormalClass>.self // expected-error {{cannot use conformance of 'NormalClass' to 'NormalProto' here; 'BADLibrary'}}
}
@inlinable public func testSpecializedConformance() {
  _ = NormalProtoAssocHolder<GenericStruct<Int>>.self // expected-error {{cannot use conformance of 'GenericStruct<T>' to 'NormalProto' here; 'BADLibrary'}}
}
