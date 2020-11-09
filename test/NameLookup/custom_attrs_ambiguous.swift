// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/custom_attrs_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/custom_attrs_B.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -I %t %s
import custom_attrs_A
import custom_attrs_B

// SR-13470

struct Test {
  @Wrapper var x: Int = 17
  // expected-error@-1 {{ambiguous use of attribute 'Wrapper'}}
  // expected-note@-2 {{use 'custom_attrs_A.' to reference the attribute 'Wrapper' in module 'custom_attrs_A'}} {{4-4=custom_attrs_A.}}
  // expected-note@-3 {{use 'custom_attrs_B.' to reference the attribute 'Wrapper' in module 'custom_attrs_B'}} {{4-4=custom_attrs_B.}}

  init(@Builder closure: () -> Int) {}
  // expected-error@-1 {{ambiguous use of attribute 'Builder'}}
  // expected-note@-2 {{use 'custom_attrs_A.' to reference the attribute 'Builder' in module 'custom_attrs_A'}} {{9-9=custom_attrs_A.}}
  // expected-note@-3 {{use 'custom_attrs_B.' to reference the attribute 'Builder' in module 'custom_attrs_B'}} {{9-9=custom_attrs_B.}}
}

