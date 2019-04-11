// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-public-helper.swift
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-helper.swift -I %t

// RUN: %target-typecheck-verify-swift -I %t

@_implementationOnly import BADLibrary
import NormalLibrary

@available(*, unavailable)
public typealias X = Int

public typealias NormalProtoAssoc<T: NormalProto> = T.Assoc
@inlinable func testConformanceInTypealias() {
  let x: NormalProtoAssoc<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  _ = x
  _ = NormalProtoAssoc<NormalStruct>() // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
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
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the implicit init.
  _ = NormalProtoAssocHolder<NormalStruct>() // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

func internalConformanceInBoundGeneric() {
  let x: NormalProtoAssocHolder<NormalStruct>? = nil // okay
  _ = x
  _ = NormalProtoAssocHolder<NormalStruct>() // okay
  _ = NormalProtoAssocHolder(nil as NormalStruct?) // okay
}

@inlinable func testDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  let alias = x is NormalProtoAssoc<NormalStruct> // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  return normal || alias
}

func internalDowncast(_ x: Any) -> Bool {
  let normal = x is NormalProtoAssocHolder<NormalStruct> // okay
  let alias = x is NormalProtoAssoc<NormalStruct> // okay
  return normal || alias
}

@inlinable func testSwitch(_ x: Any) {
  switch x {
  case let holder as NormalProtoAssocHolder<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
    _ = holder
    break
  case is NormalProtoAssoc<NormalStruct>: // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
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
  let x: NormalProtoEnumUser<NormalStruct> = .a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  _ = x
  // FIXME: We get this error twice: once for the TypeExpr and once for the case.
  _ = NormalProtoEnumUser<NormalStruct>.a // expected-error 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

func internalEnum() {
  let x: NormalProtoEnumUser<NormalStruct> = .a // okay
  _ = x
  _ = NormalProtoEnumUser<NormalStruct>.a // okay
}

@usableFromInline func testFuncImpl<T: NormalProto>(_: T.Type) {}

@inlinable func testFunc() {
  testFuncImpl(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
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
  _ = ForTestingMembers(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  _ = ForTestingMembers.init(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  _ = ForTestingMembers()[NormalStruct.self] // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  var instance = ForTestingMembers()
  instance[NormalStruct.self] = 1 // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  ForTestingMembers().method(NormalStruct.self) // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

extension NormalProtoAssocHolder {
  public static func testAnotherConformance<U: NormalProto>(_: U.Type) {}
}

@inlinable func testMultipleConformances() {
  _ = NormalProtoAssocHolder<NormalStruct>.testAnotherConformance(NormalClass.self)
 // expected-error@-1 2 {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  // expected-error@-2 {{cannot use conformance of 'NormalClass' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@inlinable func localTypeAlias() {
  typealias LocalUser = NormalProtoAssocHolder<NormalStruct> // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  typealias LocalGenericUser<T> = (T, NormalProtoAssocHolder<NormalStruct>) // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  typealias LocalProtoAssoc<T: NormalProto> = T.Assoc
  _ = LocalProtoAssoc<NormalStruct>() // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@inlinable func localFunctions() {
  func local(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  func localReturn() -> NormalProtoAssocHolder<NormalStruct> { fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  let _ = { (_: NormalProtoAssocHolder<NormalStruct>) in return } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  let _ = { () -> NormalProtoAssocHolder<NormalStruct> in fatalError() } // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@inlinable public func signatureOfInlinable(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error{{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public func testDefaultArgument(_: Int = NormalProtoAssoc<NormalStruct>()) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}


public class SubclassOfNormalClass: NormalClass {}

public func testInheritedConformance(_: NormalProtoAssocHolder<SubclassOfNormalClass>) {} // expected-error {{cannot use conformance of 'NormalClass' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public func testSpecializedConformance(_: NormalProtoAssocHolder<GenericStruct<Int>>) {} // expected-error {{cannot use conformance of 'GenericStruct<T>' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

extension Array where Element == NormalProtoAssocHolder<NormalStruct> { // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func testConstrainedExtensionUsingBadConformance() {}
}

public struct ConditionalGenericStruct<T> {}
extension ConditionalGenericStruct: NormalProto where T: NormalProto {
  public typealias Assoc = Int
}
public func testConditionalGeneric(_: NormalProtoAssocHolder<ConditionalGenericStruct<NormalStruct>>) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public protocol PublicInferredAssociatedType {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}
@usableFromInline protocol UFIInferredAssociatedType {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}
protocol InternalInferredAssociatedType {
  associatedtype Assoc: NormalProto
  func takesAssoc(_: Assoc)
}

public struct PublicInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension PublicInferredAssociatedTypeImpl: PublicInferredAssociatedType {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
extension PublicInferredAssociatedTypeImpl: UFIInferredAssociatedType {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
extension PublicInferredAssociatedTypeImpl: InternalInferredAssociatedType {} // okay

@usableFromInline struct UFIInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension UFIInferredAssociatedTypeImpl: PublicInferredAssociatedType {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
extension UFIInferredAssociatedTypeImpl: UFIInferredAssociatedType {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
extension UFIInferredAssociatedTypeImpl: InternalInferredAssociatedType {} // okay

struct InternalInferredAssociatedTypeImpl {
  public func takesAssoc(_: NormalStruct) {}
}
extension InternalInferredAssociatedTypeImpl: PublicInferredAssociatedType {} // okay
extension InternalInferredAssociatedTypeImpl: UFIInferredAssociatedType {} // okay
extension InternalInferredAssociatedTypeImpl: InternalInferredAssociatedType {} // okay


public protocol BaseProtoWithNoRequirement {
  associatedtype Assoc
  func takesAssoc(_: Assoc)
}
public protocol RefinedProto: BaseProtoWithNoRequirement where Assoc: NormalProto {
}

public struct RefinedProtoImpl: RefinedProto { // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func takesAssoc(_: NormalStruct) {}
}

public protocol RefinedSelfProto where Self: NormalProto {}
extension NormalStruct: RefinedSelfProto {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public protocol RefinedSelfProtoInheritance: NormalProto {}
extension NormalStruct: RefinedSelfProtoInheritance {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}


public protocol SlightlyMoreComplicatedRequirement {
  associatedtype Assoc: Collection where Assoc.Element: NormalProto
  func takesAssoc(_: Assoc)
}
public struct SlightlyMoreComplicatedRequirementImpl: SlightlyMoreComplicatedRequirement { // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc.Element' (inferred as 'NormalStruct'); 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func takesAssoc(_: [NormalStruct]) {}
}
public struct RequirementsHandleSubclassesToo: SlightlyMoreComplicatedRequirement { // expected-error {{cannot use conformance of 'NormalClass' to 'NormalProto' in associated type 'Self.Assoc.Element' (inferred as 'SubclassOfNormalClass'); 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func takesAssoc(_: [SubclassOfNormalClass]) {}
}

public struct RequirementsHandleSpecializationsToo: SlightlyMoreComplicatedRequirement { // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' in associated type 'Self.Assoc.Element' (inferred as 'ConditionalGenericStruct<NormalStruct>'); 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func takesAssoc(_: [ConditionalGenericStruct<NormalStruct>]) {}
}

public struct ClassConstrainedGenericArg<T: NormalClass>: PublicInferredAssociatedType { // expected-error {{cannot use conformance of 'NormalClass' to 'NormalProto' in associated type 'Self.Assoc' (inferred as 'T'); 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func takesAssoc(_: T) {}
}


public protocol RecursiveRequirements {
  associatedtype Other: RecursiveRequirements
}
extension GenericStruct: RecursiveRequirements {
  public typealias Other = GenericStruct<T>
}
public struct RecursiveRequirementsHolder<T: RecursiveRequirements> {}
public func makeSureRecursiveRequirementsDontBreakEverything(_: RecursiveRequirementsHolder<GenericStruct<Int>>) {}


@inlinable func testFunctionBody() {
  
}
