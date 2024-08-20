// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberImportVisibility/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -package-name TestPackage %S/Inputs/MemberImportVisibility/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/MemberImportVisibility/members_C.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -swift-version 5 -package-name TestPackage
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -swift-version 6 -package-name TestPackage
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -swift-version 5 -package-name TestPackage -enable-experimental-feature MemberImportVisibility -verify-additional-prefix member-visibility-

import members_C
// expected-member-visibility-note 12{{add import of module 'members_B'}}{{1-1=internal import members_B\n}}


func testExtensionMembers(x: X, y: Y<Z>) {
  x.XinA()
  y.YinA()

  x.XinB() // expected-member-visibility-error{{instance method 'XinB()' is not available due to missing import of defining module 'members_B'}}
  x.XinB_package() // expected-member-visibility-error{{instance method 'XinB_package()' is not available due to missing import of defining module 'members_B'}}
  y.YinB() // expected-member-visibility-error{{instance method 'YinB()' is not available due to missing import of defining module 'members_B'}}

  x.XinC()
  y.YinC()
}

func testOperatorMembers(x: X, y: Y<Z>) {
  _ = x <<< x
  _ = y <<< y

  _ = x >>> x // expected-error{{cannot find operator '>>>' in scope}}
  _ = y >>> y // expected-error{{cannot find operator '>>>' in scope}}

  _ = x <> x
  _ = y <> y
}

extension X {
  var testProperties: (Bool, Bool, Bool, Bool) {
    return (
      propXinA,
      propXinB, // expected-member-visibility-error{{property 'propXinB' is not available due to missing import of defining module 'members_B'}}
      propXinB_package, // expected-member-visibility-error{{property 'propXinB_package' is not available due to missing import of defining module 'members_B}}
      propXinC
    )
  }

  func testNestedTypes() {
    _ = NestedInA.self
    _ = NestedInB.self // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
    _ = NestedInB_package.self // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
    _ = NestedInC.self
  }

  var nestedInA: NestedInA { fatalError() }
  var nestedInB: NestedInB { fatalError() } // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
  var nestedInB_package: NestedInB_package { fatalError() } // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
  var nestedInC: NestedInC { fatalError() }
}

extension X.NestedInA {}
extension X.NestedInB {} // expected-member-visibility-error{{struct 'NestedInB' is not available due to missing import of defining module 'members_B'}}
extension X.NestedInB_package {} // expected-member-visibility-error{{struct 'NestedInB_package' is not available due to missing import of defining module 'members_B'}}
extension X.NestedInC {}

func testTopLevelTypes() {
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

struct ConformsToProtocolInA: ProtocolInA {} // expected-member-visibility-error{{type 'ConformsToProtocolInA' does not conform to protocol 'ProtocolInA'}} expected-member-visibility-note {{add stubs for conformance}}
