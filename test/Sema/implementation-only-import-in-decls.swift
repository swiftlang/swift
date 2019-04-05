// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-public-helper.swift
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-helper.swift -I %t

// RUN: %target-typecheck-verify-swift -I %t

import NormalLibrary
@_implementationOnly import BADLibrary

public struct TestConformance: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

@usableFromInline struct TestConformanceUFI: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

struct TestConformanceOkay: BadProto {} // ok

public class TestConformanceClass: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public enum TestConformanceEnum: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}


public struct TestGenericParams<T: BadProto> {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public struct TestGenericParamsWhereClause<T> where T: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public enum TestCase {
  case x(BadStruct) // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  case y(Int, BadStruct) // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public func testGenericParams<T: BadProto>(_: T) {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public func testGenericParamsWhereClause<T>(_: T) where T: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public func testParams(_: BadStruct, _: BadProto) {}
// expected-error@-1 {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
// expected-error@-2 {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public func testResult() -> BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public struct TestSubscript {
  public subscript<T: BadProto>(_: T) -> Int { return 0 } // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public subscript<T>(where _: T) -> Int where T: BadProto { return 0 } // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public subscript(_: BadStruct) -> Int { return 0 } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public subscript(_: Int) -> BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public struct TestInit {
  public init(_: BadStruct) {} // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public init<T: BadProto>(_: T) {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public init<T>(where _: T) where T: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public protocol TestInherited: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public protocol TestConstraintBase {
  associatedtype Assoc
}
public protocol TestConstraint: TestConstraintBase where Assoc: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public protocol TestConstraintEq: TestConstraintBase where Assoc == BadStruct {} // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public protocol TestAssocType {
  associatedtype Assoc: BadProto // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public protocol TestAssocTypeWhereClause {
  associatedtype Assoc: Collection where Assoc.Element: BadProto // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public enum TestRawType: IntLike { // expected-error {{cannot use 'IntLike' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  case x = 1
}

public class TestSubclass: BadClass { // expected-error {{cannot use 'BadClass' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public typealias TestUnderlying = BadStruct // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public typealias TestGenericParamsAliasWhereClause<T> = T where T: BadProto // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public typealias TestGenericParamsAlias<T: BadProto> = T // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public var testBadType: BadStruct? = nil // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var testBadTypeInferred = Optional<BadStruct>.none // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var testBadTypePartiallyInferred: Optional = Optional<BadStruct>.none // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var (testBadTypeTuple1, testBadTypeTuple2): (BadStruct?, BadClass?) = (nil, nil)
// expected-error@-1 {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
// expected-error@-2 {{cannot use 'BadClass' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var (testBadTypeTuplePartlyInferred1, testBadTypeTuplePartlyInferred2): (Optional, Optional) = (Optional<Int>.none, Optional<BadStruct>.none) // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var (testBadTypeTuplePartlyInferred3, testBadTypeTuplePartlyInferred4): (Optional, Optional) = (Optional<BadStruct>.none, Optional<Int>.none) // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var testMultipleBindings1: Int? = nil, testMultipleBindings2: BadStruct? = nil // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
public var testMultipleBindings3: BadStruct? = nil, testMultipleBindings4: Int? = nil // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

extension BadStruct { // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func testExtensionOfBadType() {} // FIXME: Should complain here instead of at the extension decl.
}
extension BadStruct {
  func testExtensionOfOkayType() {}
}

extension Array where Element == BadStruct { // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  public func testExtensionWithBadRequirement() {} // FIXME: Should complain here instead of at the extension decl.
}

extension Array where Element == BadStruct {
  func testExtensionWithOkayRequirement() {} // okay
}

extension Int: BadProto {} // expected-error {{cannot use 'BadProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
struct TestExtensionConformanceOkay {}
extension TestExtensionConformanceOkay: BadProto {} // okay

public protocol TestConstrainedExtensionProto {}
extension Array: TestConstrainedExtensionProto where Element == BadStruct { // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}


infix operator !!!!!!: BadPrecedence // expected-error {{cannot use 'BadPrecedence' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

precedencegroup TestLowerThan {
  lowerThan: BadPrecedence // expected-error {{cannot use 'BadPrecedence' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}
precedencegroup TestHigherThan {
  higherThan: BadPrecedence // expected-error {{cannot use 'BadPrecedence' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public struct PublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar = [BadStruct]() // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@usableFromInline internal struct UFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar = [BadStruct]() // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public class PublicClassStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar = [BadStruct]() // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public typealias NormalProtoAssoc<T: NormalProto> = T.Assoc
public func testConformanceInTypealias(_: NormalProtoAssoc<NormalStruct>) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

public struct NormalProtoAssocHolder<T: NormalProto> {
  public var value: T.Assoc
}
public func testConformanceInBoundGeneric(_: NormalProtoAssocHolder<NormalStruct>) {} // expected-error {{cannot use conformance of 'NormalStruct' to 'NormalProto' here; 'BADLibrary' has been imported as '@_implementationOnly'}}

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
