// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberImportVisibility/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t -package-name TestPackage %S/Inputs/MemberImportVisibility/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/MemberImportVisibility/members_C.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility:migrate

// REQUIRES: swift_feature_MemberImportVisibility

import members_C
// expected-note 3 {{add import of module 'members_B'}}{{1-1=internal import members_B\n}}


func testMigration(x: X, y: Y<Z>) {
  x.XinA()
  y.YinA()

  x.XinB() // expected-warning {{instance method 'XinB()' is not available due to missing import of defining module 'members_B'}}
  y.YinB() // expected-warning{{instance method 'YinB()' is not available due to missing import of defining module 'members_B'}}

  x.XinC()
  y.YinC()

  _ = X(true)
  _ = X(1) // expected-warning {{initializer 'init(_:)' is not available due to missing import of defining module 'members_B'}}
}
