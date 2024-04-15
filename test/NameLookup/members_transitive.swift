// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/members_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/members_B.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/members_C.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -enable-experimental-feature MemberImportVisibility

import members_C
// expected-note 2{{add import of module 'members_B'}}{{1-1=import members_B\n}}
func test(x: X, y: Y<Z>) {
  // Declared in members_A
  x.XinA()
  y.YinA()
  _ = x <<< x
  _ = y <<< y

  // Declared in members_B
  x.XinB() // expected-error{{instance method 'XinB()' is not available due to missing import of defining module 'members_B'}}
  y.YinB() // expected-error{{instance method 'YinB()' is not available due to missing import of defining module 'members_B'}}
  _ = x >>> x // expected-error{{cannot find operator '>>>' in scope}}
  _ = y >>> y // expected-error{{cannot find operator '>>>' in scope}}

  // Declared in members_C
  x.XinC()
  y.YinC()
  _ = x <> x
  _ = y <> y
}
