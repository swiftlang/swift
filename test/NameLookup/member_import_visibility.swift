// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberImportVisibility/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -package-name TestPackage %S/Inputs/MemberImportVisibility/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/MemberImportVisibility/members_C.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -verify-ignore-unrelated -swift-version 5 -package-name TestPackage -verify-additional-prefix ambiguity-
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -verify-ignore-unrelated -swift-version 6 -package-name TestPackage -verify-additional-prefix ambiguity-
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -verify-ignore-unrelated -swift-version 5 -package-name TestPackage -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-visibility-

// REQUIRES: swift_feature_MemberImportVisibility

import members_C
// expected-member-visibility-note 28{{add import of module 'members_B'}}{{1-1=internal import members_B\n}}


func testExtensionMembers(x: X, y: Y<Z>) {
  x.XinA()
  x.XinA_spi() // expected-error{{'XinA_spi' is inaccessible due to '@_spi' protection level}}
  y.YinA()

  x.XinB() // expected-member-visibility-error{{instance method 'XinB()' is not available due to missing import of defining module 'members_B'}}
  x.XinB_package() // expected-member-visibility-error{{instance method 'XinB_package()' is not available due to missing import of defining module 'members_B'}}
  x.XinB_spi() // expected-error{{'XinB_spi' is inaccessible due to '@_spi' protection level}}
  y.YinB() // expected-member-visibility-error{{instance method 'YinB()' is not available due to missing import of defining module 'members_B'}}

  x.XinC()
  x.XinC_spi() // expected-error{{'XinC_spi' is inaccessible due to '@_spi' protection level}}
  y.YinC()

  _ = X(true)
  _ = X(1) // expected-member-visibility-error{{initializer 'init(_:)' is not available due to missing import of defining module 'members_B'}}

  _ = x.ambiguous() // expected-ambiguity-error{{ambiguous use of 'ambiguous()'}}
  let _: Bool = x.ambiguous()
  let _: Int = x.ambiguous() // expected-member-visibility-error{{instance method 'ambiguous()' is not available due to missing import of defining module 'members_B'}}

  // ambiguousDisfavored() has two overloads:
  // - members_B: public func ambiguousDisfavored() -> Int
  // - members_C: @_disfavoredOverload public func ambiguousDisfavored() -> Bool
  // With MemberImportVisibility, the overload from members_C should be picked.
  // Otherwise, the overload from members_B should be picked.
  let disfavoredResult = x.ambiguousDisfavored()
  let _: Bool = disfavoredResult // expected-ambiguity-error{{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
  let _: Int = disfavoredResult // expected-member-visibility-error{{cannot convert value of type 'Bool' to specified type 'Int'}}
  let _: Bool = x.ambiguousDisfavored()
  let _: Int = x.ambiguousDisfavored() // expected-member-visibility-error{{instance method 'ambiguousDisfavored()' is not available due to missing import of defining module 'members_B'}}

  func takesKeyPath<T, U>(_ t: T, _ keyPath: KeyPath<T, U>) -> () { }

  takesKeyPath(x, \.propXinA)
  takesKeyPath(x, \.propXinB) // expected-member-visibility-warning{{property 'propXinB' is not available due to missing import of defining module 'members_B'}}
  takesKeyPath(x, \.propXinC)

  takesKeyPath(x, \.propXinA.description)
  takesKeyPath(x, \.propXinB.description) // expected-member-visibility-warning{{property 'propXinB' is not available due to missing import of defining module 'members_B'}}
  takesKeyPath(x, \.propXinC.description)
}

func testOperatorMembers(x: X, y: Y<Z>) {
  _ = x <<< x
  _ = y <<< y

  _ = x >>> x // expected-error{{cannot find operator '>>>' in scope}}
  _ = y >>> y // expected-error{{cannot find operator '>>>' in scope}}

  _ = x <> x
  _ = y <> y
}

struct GenericType<T> { }

extension X {
  var testPropertyInA: Bool { propXinA }
  var testPropertyInB: Bool { propXinB } // expected-member-visibility-error {{property 'propXinB' is not available due to missing import of defining module 'members_B'}}
  var testPropertyInB_package: Bool { propXinB_package } // expected-member-visibility-error{{property 'propXinB_package' is not available due to missing import of defining module 'members_B}}
  var testPropertyInC: Bool { propXinC }

  // This is not diagnosed in either mode (the property from the nearest scope is always preferred
  var testAmbiguousProp: Bool { ambiguousProp }

  func testTypeExpressions() {
    _ = NestedInA.self
    _ = NestedInB.self // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
    _ = NestedInB_package.self // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
    _ = (NestedInB).self // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
    _ = (NestedInA, NestedInB, NestedInC).self // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
    _ = GenericType<NestedInB>.self // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
    _ = NestedInC.self
    _ = AmbiguousNestedType.self // expected-ambiguity-error{{ambiguous use of 'AmbiguousNestedType'}}
  }

  var hasNestedInAType: NestedInA { fatalError() }
  var hasNestedInBType: NestedInB { fatalError() } // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
  var hasNestedInB_packageType: NestedInB_package { fatalError() } // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
  var hasNestedInBTrivialTupleType: (NestedInB) { fatalError() } // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
  var hasTupleTypeContainingNestedInB: (NestedInA, NestedInB, NestedInC) { fatalError() } // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
  var hasNestedInBAsGenericTypeParameter: GenericType<NestedInB> { fatalError() } // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
  var hasNestedInCType: NestedInC { fatalError() }

  func hasNestedInAInGenericReqs<T>(_ t: T) where T: ProtoNestedInA { }
  func hasNestedInBInGenericReqs<T>(_ t: T) where T: ProtoNestedInB { } // expected-member-visibility-error{{protocol 'ProtoNestedInB' is not available due to missing import of defining module 'members_B'}}
  func hasNestedInCInGenericReqs<T>(_ t: T) where T: ProtoNestedInC { }

  func testMembersShadowingGlobals() {
    shadowedByMemberOnXinA()
    shadowedByMemberOnXinB() // expected-member-visibility-warning{{'shadowedByMemberOnXinB()' is deprecated}}
    shadowedByMemberOnXinC()

    _ = max(0, 1) // expected-ambiguity-error{{static member 'max' can only be used on the type 'X', not on the instance self}}
    // expected-ambiguity-error@-1{{cannot call value of non-function type 'Int'}}
  }

  static func testStaticMembersShadowingGlobals() {
    shadowedByStaticMemberOnXinA()
    shadowedByStaticMemberOnXinB() // expected-member-visibility-warning{{'shadowedByStaticMemberOnXinB()' is deprecated}}
    shadowedByStaticMemberOnXinC()

    _ = max(0, 1) // expected-ambiguity-error{{use of 'max' refers to instance method rather than global function 'max' in module 'Swift'}}
    // expected-ambiguity-note@-1{{use 'Swift.' to reference the global function in module 'Swift'}}
  }
}

extension X.NestedInA {}
extension X.NestedInB {} // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
extension X.NestedInB_package {} // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
extension X.NestedInC {}

func testTypeExpressionsReferencingTopLevelTypes() {
  _ = EnumInA.self
  _ = EnumInB.self // expected-error{{cannot find 'EnumInB' in scope}}
  _ = EnumInB_package.self // expected-error{{cannot find 'EnumInB_package' in scope}}
  _ = EnumInC.self
}

class DerivedFromClassInC: DerivedClassInC {
  override func methodInA() {}
  override func methodInB() {} // expected-member-visibility-error{{instance method 'methodInB()' is not available due to missing import of defining module 'members_B'}}
  override func methodInC() {}
}

// FIXME: Visibility of defaultedRequirementInB() should be diagnosed (rdar://154237873)
struct ConformsToProtocolInA: ProtocolInA {} // expected-member-visibility-error{{type 'ConformsToProtocolInA' does not conform to protocol 'ProtocolInA'}} expected-member-visibility-note {{add stubs for conformance}}{{44-44=\n    func defaultedRequirementInB() {\n        <#code#>\n    \}\n}}

func testInheritedMethods(
  a: BaseClassInA,
  c: DerivedClassInC,
) {
  let b = c.asDerivedClassInB()

  a.methodInA()
  b.methodInA()
  c.methodInA()

  b.methodInB() // expected-member-visibility-error{{instance method 'methodInB()' is not available due to missing import of defining module 'members_B'}}
  c.methodInB() // expected-member-visibility-error{{instance method 'methodInB()' is not available due to missing import of defining module 'members_B'}}

  c.methodInC()

  a.overriddenMethod()
  b.overriddenMethod() // expected-member-visibility-error{{instance method 'overriddenMethod()' is not available due to missing import of defining module 'members_B'}}
  c.overriddenMethod()
}

func testLeadingDotSyntax() {
  func takesP<T: P>(_: T) { }
  takesP(.zInA)
  takesP(.zInB) // expected-member-visibility-error{{static property 'zInB' is not available due to missing import of defining module 'members_B'}}
  takesP(.zInC)
  takesP(.zAmbiguous)
}

func testConformanceMember(_ h1: HasEquatableMembers, _ h2: HasEquatableMembers) {
  _ = h1.a == h2.a
  // Technically, this references the EquatableInB.== member that hasn't been
  // imported. However, the conformance of EquatableInB: Equatable is visible
  // here because conformances are not yet subject to import visibility rules.
  // As a result, the == requirement is technically visible and therefore there
  // should be no diagnostic with MemberImportVisibility enabled.
  _ = h1.b == h2.b
  _ = h1.c == h2.c
}
